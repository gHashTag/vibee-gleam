// VIBEE P2P Escrow Smart Contract
// Tact implementation for TON blockchain
// Supports TON and USDT (Jetton) escrow for P2P trading

import "@stdlib/deploy";
import "@stdlib/ownable";

// =============================================================================
// MESSAGES
// =============================================================================

message CreateOrder {
    orderId: Int as uint64;
    fiatAmount: Int as uint64;      // Fiat amount in cents (e.g., 350000 = 3500.00 THB)
    fiatCurrency: Int as uint8;      // 1=THB, 2=RUB, 3=USD, 4=EUR
    paymentTimeout: Int as uint32;   // Timeout in seconds (e.g., 1800 = 30 min)
}

message TakeOrder {
    orderId: Int as uint64;
}

message ConfirmPayment {
    orderId: Int as uint64;
}

message OracleConfirm {
    orderId: Int as uint64;
    signature: Slice;
}

message CancelOrder {
    orderId: Int as uint64;
}

message OpenDispute {
    orderId: Int as uint64;
    reason: Int as uint8;
}

message ResolveDispute {
    orderId: Int as uint64;
    winner: Address;              // Who gets the crypto
}

message WithdrawFees {
    amount: Int as coins;
}

message UpdateOracle {
    newOracle: Address;
}

message UpdateFee {
    newFeePercent: Int as uint16; // Fee in basis points (50 = 0.5%)
}

// =============================================================================
// ORDER STATUS
// =============================================================================

const STATUS_OPEN: Int = 0;
const STATUS_LOCKED: Int = 1;
const STATUS_FIAT_SENT: Int = 2;
const STATUS_COMPLETED: Int = 3;
const STATUS_DISPUTED: Int = 4;
const STATUS_CANCELLED: Int = 5;
const STATUS_EXPIRED: Int = 6;

// =============================================================================
// ORDER STRUCT
// =============================================================================

struct Order {
    seller: Address;
    buyer: Address?;
    cryptoAmount: Int as coins;
    fiatAmount: Int as uint64;
    fiatCurrency: Int as uint8;
    status: Int as uint8;
    createdAt: Int as uint32;
    lockedAt: Int as uint32;
    paymentTimeout: Int as uint32;
}

// =============================================================================
// CONTRACT
// =============================================================================

contract P2PEscrow with Deployable, Ownable {
    owner: Address;
    oracle: Address;
    feePercent: Int as uint16;     // Fee in basis points (50 = 0.5%)
    collectedFees: Int as coins;
    orderCounter: Int as uint64;

    // Orders storage (map orderId -> Order)
    orders: map<Int, Order>;

    init(oracle: Address) {
        self.owner = sender();
        self.oracle = oracle;
        self.feePercent = 50;      // 0.5% default fee
        self.collectedFees = 0;
        self.orderCounter = 0;
    }

    // =========================================================================
    // CREATE ORDER (Seller deposits crypto)
    // =========================================================================

    receive(msg: CreateOrder) {
        let ctx: Context = context();
        let depositAmount: Int = ctx.value;

        // Minimum deposit check (0.1 TON minimum)
        require(depositAmount >= ton("0.1"), "Deposit too small");

        // Create new order
        let order: Order = Order{
            seller: ctx.sender,
            buyer: null,
            cryptoAmount: depositAmount,
            fiatAmount: msg.fiatAmount,
            fiatCurrency: msg.fiatCurrency,
            status: STATUS_OPEN,
            createdAt: now(),
            lockedAt: 0,
            paymentTimeout: msg.paymentTimeout
        };

        // Store order
        self.orders.set(msg.orderId, order);
        self.orderCounter = self.orderCounter + 1;

        // Emit event (for indexers)
        emit(OrderCreated{
            orderId: msg.orderId,
            seller: ctx.sender,
            cryptoAmount: depositAmount,
            fiatAmount: msg.fiatAmount
        }.toCell());
    }

    // =========================================================================
    // TAKE ORDER (Buyer claims order)
    // =========================================================================

    receive(msg: TakeOrder) {
        let ctx: Context = context();
        let order: Order? = self.orders.get(msg.orderId);

        require(order != null, "Order not found");
        let o: Order = order!!;

        require(o.status == STATUS_OPEN, "Order not available");
        require(o.seller != ctx.sender, "Cannot buy own order");

        // Update order with buyer info
        let updatedOrder: Order = Order{
            seller: o.seller,
            buyer: ctx.sender,
            cryptoAmount: o.cryptoAmount,
            fiatAmount: o.fiatAmount,
            fiatCurrency: o.fiatCurrency,
            status: STATUS_LOCKED,
            createdAt: o.createdAt,
            lockedAt: now(),
            paymentTimeout: o.paymentTimeout
        };

        self.orders.set(msg.orderId, updatedOrder);

        emit(OrderTaken{
            orderId: msg.orderId,
            buyer: ctx.sender
        }.toCell());
    }

    // =========================================================================
    // CONFIRM PAYMENT (Seller confirms fiat received)
    // =========================================================================

    receive(msg: ConfirmPayment) {
        let ctx: Context = context();
        let order: Order? = self.orders.get(msg.orderId);

        require(order != null, "Order not found");
        let o: Order = order!!;

        // Only seller can confirm
        require(o.seller == ctx.sender, "Only seller can confirm");
        require(o.status == STATUS_LOCKED || o.status == STATUS_FIAT_SENT, "Order not locked");

        // Release crypto to buyer
        self.releaseOrder(msg.orderId, o);
    }

    // =========================================================================
    // ORACLE CONFIRM (Automatic confirmation)
    // =========================================================================

    receive(msg: OracleConfirm) {
        let ctx: Context = context();

        // Only oracle can call this
        require(ctx.sender == self.oracle, "Only oracle can confirm");

        let order: Order? = self.orders.get(msg.orderId);
        require(order != null, "Order not found");
        let o: Order = order!!;

        require(o.status == STATUS_LOCKED || o.status == STATUS_FIAT_SENT, "Order not locked");

        // TODO: Verify signature in production
        // For now, trust oracle address

        // Release crypto to buyer
        self.releaseOrder(msg.orderId, o);
    }

    // =========================================================================
    // RELEASE ORDER (Internal function)
    // =========================================================================

    fun releaseOrder(orderId: Int, o: Order) {
        require(o.buyer != null, "No buyer");

        // Calculate fee
        let fee: Int = (o.cryptoAmount * self.feePercent) / 10000;
        let buyerAmount: Int = o.cryptoAmount - fee;

        // Collect fee
        self.collectedFees = self.collectedFees + fee;

        // Update order status
        let completedOrder: Order = Order{
            seller: o.seller,
            buyer: o.buyer,
            cryptoAmount: o.cryptoAmount,
            fiatAmount: o.fiatAmount,
            fiatCurrency: o.fiatCurrency,
            status: STATUS_COMPLETED,
            createdAt: o.createdAt,
            lockedAt: o.lockedAt,
            paymentTimeout: o.paymentTimeout
        };
        self.orders.set(orderId, completedOrder);

        // Send crypto to buyer
        send(SendParameters{
            to: o.buyer!!,
            value: buyerAmount,
            mode: SendIgnoreErrors,
            body: "P2P Escrow Release".asComment()
        });

        emit(OrderCompleted{
            orderId: orderId,
            buyer: o.buyer!!,
            amount: buyerAmount
        }.toCell());
    }

    // =========================================================================
    // CANCEL ORDER (Seller can cancel if not locked)
    // =========================================================================

    receive(msg: CancelOrder) {
        let ctx: Context = context();
        let order: Order? = self.orders.get(msg.orderId);

        require(order != null, "Order not found");
        let o: Order = order!!;

        // Only seller can cancel open orders
        require(o.seller == ctx.sender, "Only seller can cancel");
        require(o.status == STATUS_OPEN, "Can only cancel open orders");

        // Update status
        let cancelledOrder: Order = Order{
            seller: o.seller,
            buyer: o.buyer,
            cryptoAmount: o.cryptoAmount,
            fiatAmount: o.fiatAmount,
            fiatCurrency: o.fiatCurrency,
            status: STATUS_CANCELLED,
            createdAt: o.createdAt,
            lockedAt: o.lockedAt,
            paymentTimeout: o.paymentTimeout
        };
        self.orders.set(msg.orderId, cancelledOrder);

        // Refund seller
        send(SendParameters{
            to: o.seller,
            value: o.cryptoAmount,
            mode: SendIgnoreErrors,
            body: "P2P Escrow Refund".asComment()
        });

        emit(OrderCancelled{
            orderId: msg.orderId
        }.toCell());
    }

    // =========================================================================
    // EXPIRE ORDER (Anyone can trigger for timed-out orders)
    // =========================================================================

    receive("expire") {
        // This is a simple trigger - iterate through locked orders and expire them
        // In production, this should be called by an off-chain worker
    }

    // =========================================================================
    // DISPUTE
    // =========================================================================

    receive(msg: OpenDispute) {
        let ctx: Context = context();
        let order: Order? = self.orders.get(msg.orderId);

        require(order != null, "Order not found");
        let o: Order = order!!;

        // Only buyer or seller can open dispute
        require(o.seller == ctx.sender || o.buyer == ctx.sender, "Not a party");
        require(o.status == STATUS_LOCKED || o.status == STATUS_FIAT_SENT, "Invalid status");

        let disputedOrder: Order = Order{
            seller: o.seller,
            buyer: o.buyer,
            cryptoAmount: o.cryptoAmount,
            fiatAmount: o.fiatAmount,
            fiatCurrency: o.fiatCurrency,
            status: STATUS_DISPUTED,
            createdAt: o.createdAt,
            lockedAt: o.lockedAt,
            paymentTimeout: o.paymentTimeout
        };
        self.orders.set(msg.orderId, disputedOrder);

        emit(DisputeOpened{
            orderId: msg.orderId,
            openedBy: ctx.sender
        }.toCell());
    }

    receive(msg: ResolveDispute) {
        let ctx: Context = context();

        // Only owner can resolve disputes
        require(ctx.sender == self.owner, "Only owner can resolve");

        let order: Order? = self.orders.get(msg.orderId);
        require(order != null, "Order not found");
        let o: Order = order!!;

        require(o.status == STATUS_DISPUTED, "Not disputed");

        // Verify winner is buyer or seller
        require(msg.winner == o.seller || msg.winner == o.buyer, "Invalid winner");

        // Update status
        let resolvedOrder: Order = Order{
            seller: o.seller,
            buyer: o.buyer,
            cryptoAmount: o.cryptoAmount,
            fiatAmount: o.fiatAmount,
            fiatCurrency: o.fiatCurrency,
            status: STATUS_COMPLETED,
            createdAt: o.createdAt,
            lockedAt: o.lockedAt,
            paymentTimeout: o.paymentTimeout
        };
        self.orders.set(msg.orderId, resolvedOrder);

        // Send crypto to winner
        send(SendParameters{
            to: msg.winner,
            value: o.cryptoAmount,
            mode: SendIgnoreErrors,
            body: "P2P Dispute Resolution".asComment()
        });

        emit(DisputeResolved{
            orderId: msg.orderId,
            winner: msg.winner
        }.toCell());
    }

    // =========================================================================
    // ADMIN FUNCTIONS
    // =========================================================================

    receive(msg: WithdrawFees) {
        self.requireOwner();
        require(msg.amount <= self.collectedFees, "Not enough fees");

        self.collectedFees = self.collectedFees - msg.amount;

        send(SendParameters{
            to: self.owner,
            value: msg.amount,
            mode: SendIgnoreErrors,
            body: "P2P Fees Withdrawal".asComment()
        });
    }

    receive(msg: UpdateOracle) {
        self.requireOwner();
        self.oracle = msg.newOracle;
    }

    receive(msg: UpdateFee) {
        self.requireOwner();
        require(msg.newFeePercent <= 500, "Fee too high"); // Max 5%
        self.feePercent = msg.newFeePercent;
    }

    // =========================================================================
    // GETTERS
    // =========================================================================

    get fun getOrder(orderId: Int): Order? {
        return self.orders.get(orderId);
    }

    get fun getOrderCount(): Int {
        return self.orderCounter;
    }

    get fun getCollectedFees(): Int {
        return self.collectedFees;
    }

    get fun getFeePercent(): Int {
        return self.feePercent;
    }

    get fun getOracle(): Address {
        return self.oracle;
    }
}

// =============================================================================
// EVENTS
// =============================================================================

message OrderCreated {
    orderId: Int as uint64;
    seller: Address;
    cryptoAmount: Int as coins;
    fiatAmount: Int as uint64;
}

message OrderTaken {
    orderId: Int as uint64;
    buyer: Address;
}

message OrderCompleted {
    orderId: Int as uint64;
    buyer: Address;
    amount: Int as coins;
}

message OrderCancelled {
    orderId: Int as uint64;
}

message DisputeOpened {
    orderId: Int as uint64;
    openedBy: Address;
}

message DisputeResolved {
    orderId: Int as uint64;
    winner: Address;
}
