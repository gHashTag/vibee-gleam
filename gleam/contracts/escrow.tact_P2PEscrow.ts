import {
    Cell,
    Slice,
    Address,
    Builder,
    beginCell,
    ComputeError,
    TupleItem,
    TupleReader,
    Dictionary,
    contractAddress,
    address,
    ContractProvider,
    Sender,
    Contract,
    ContractABI,
    ABIType,
    ABIGetter,
    ABIReceiver,
    TupleBuilder,
    DictionaryValue
} from '@ton/core';

export type DataSize = {
    $$type: 'DataSize';
    cells: bigint;
    bits: bigint;
    refs: bigint;
}

export function storeDataSize(src: DataSize) {
    return (builder: Builder) => {
        const b_0 = builder;
        b_0.storeInt(src.cells, 257);
        b_0.storeInt(src.bits, 257);
        b_0.storeInt(src.refs, 257);
    };
}

export function loadDataSize(slice: Slice) {
    const sc_0 = slice;
    const _cells = sc_0.loadIntBig(257);
    const _bits = sc_0.loadIntBig(257);
    const _refs = sc_0.loadIntBig(257);
    return { $$type: 'DataSize' as const, cells: _cells, bits: _bits, refs: _refs };
}

export function loadTupleDataSize(source: TupleReader) {
    const _cells = source.readBigNumber();
    const _bits = source.readBigNumber();
    const _refs = source.readBigNumber();
    return { $$type: 'DataSize' as const, cells: _cells, bits: _bits, refs: _refs };
}

export function loadGetterTupleDataSize(source: TupleReader) {
    const _cells = source.readBigNumber();
    const _bits = source.readBigNumber();
    const _refs = source.readBigNumber();
    return { $$type: 'DataSize' as const, cells: _cells, bits: _bits, refs: _refs };
}

export function storeTupleDataSize(source: DataSize) {
    const builder = new TupleBuilder();
    builder.writeNumber(source.cells);
    builder.writeNumber(source.bits);
    builder.writeNumber(source.refs);
    return builder.build();
}

export function dictValueParserDataSize(): DictionaryValue<DataSize> {
    return {
        serialize: (src, builder) => {
            builder.storeRef(beginCell().store(storeDataSize(src)).endCell());
        },
        parse: (src) => {
            return loadDataSize(src.loadRef().beginParse());
        }
    }
}

export type SignedBundle = {
    $$type: 'SignedBundle';
    signature: Buffer;
    signedData: Slice;
}

export function storeSignedBundle(src: SignedBundle) {
    return (builder: Builder) => {
        const b_0 = builder;
        b_0.storeBuffer(src.signature);
        b_0.storeBuilder(src.signedData.asBuilder());
    };
}

export function loadSignedBundle(slice: Slice) {
    const sc_0 = slice;
    const _signature = sc_0.loadBuffer(64);
    const _signedData = sc_0;
    return { $$type: 'SignedBundle' as const, signature: _signature, signedData: _signedData };
}

export function loadTupleSignedBundle(source: TupleReader) {
    const _signature = source.readBuffer();
    const _signedData = source.readCell().asSlice();
    return { $$type: 'SignedBundle' as const, signature: _signature, signedData: _signedData };
}

export function loadGetterTupleSignedBundle(source: TupleReader) {
    const _signature = source.readBuffer();
    const _signedData = source.readCell().asSlice();
    return { $$type: 'SignedBundle' as const, signature: _signature, signedData: _signedData };
}

export function storeTupleSignedBundle(source: SignedBundle) {
    const builder = new TupleBuilder();
    builder.writeBuffer(source.signature);
    builder.writeSlice(source.signedData.asCell());
    return builder.build();
}

export function dictValueParserSignedBundle(): DictionaryValue<SignedBundle> {
    return {
        serialize: (src, builder) => {
            builder.storeRef(beginCell().store(storeSignedBundle(src)).endCell());
        },
        parse: (src) => {
            return loadSignedBundle(src.loadRef().beginParse());
        }
    }
}

export type StateInit = {
    $$type: 'StateInit';
    code: Cell;
    data: Cell;
}

export function storeStateInit(src: StateInit) {
    return (builder: Builder) => {
        const b_0 = builder;
        b_0.storeRef(src.code);
        b_0.storeRef(src.data);
    };
}

export function loadStateInit(slice: Slice) {
    const sc_0 = slice;
    const _code = sc_0.loadRef();
    const _data = sc_0.loadRef();
    return { $$type: 'StateInit' as const, code: _code, data: _data };
}

export function loadTupleStateInit(source: TupleReader) {
    const _code = source.readCell();
    const _data = source.readCell();
    return { $$type: 'StateInit' as const, code: _code, data: _data };
}

export function loadGetterTupleStateInit(source: TupleReader) {
    const _code = source.readCell();
    const _data = source.readCell();
    return { $$type: 'StateInit' as const, code: _code, data: _data };
}

export function storeTupleStateInit(source: StateInit) {
    const builder = new TupleBuilder();
    builder.writeCell(source.code);
    builder.writeCell(source.data);
    return builder.build();
}

export function dictValueParserStateInit(): DictionaryValue<StateInit> {
    return {
        serialize: (src, builder) => {
            builder.storeRef(beginCell().store(storeStateInit(src)).endCell());
        },
        parse: (src) => {
            return loadStateInit(src.loadRef().beginParse());
        }
    }
}

export type Context = {
    $$type: 'Context';
    bounceable: boolean;
    sender: Address;
    value: bigint;
    raw: Slice;
}

export function storeContext(src: Context) {
    return (builder: Builder) => {
        const b_0 = builder;
        b_0.storeBit(src.bounceable);
        b_0.storeAddress(src.sender);
        b_0.storeInt(src.value, 257);
        b_0.storeRef(src.raw.asCell());
    };
}

export function loadContext(slice: Slice) {
    const sc_0 = slice;
    const _bounceable = sc_0.loadBit();
    const _sender = sc_0.loadAddress();
    const _value = sc_0.loadIntBig(257);
    const _raw = sc_0.loadRef().asSlice();
    return { $$type: 'Context' as const, bounceable: _bounceable, sender: _sender, value: _value, raw: _raw };
}

export function loadTupleContext(source: TupleReader) {
    const _bounceable = source.readBoolean();
    const _sender = source.readAddress();
    const _value = source.readBigNumber();
    const _raw = source.readCell().asSlice();
    return { $$type: 'Context' as const, bounceable: _bounceable, sender: _sender, value: _value, raw: _raw };
}

export function loadGetterTupleContext(source: TupleReader) {
    const _bounceable = source.readBoolean();
    const _sender = source.readAddress();
    const _value = source.readBigNumber();
    const _raw = source.readCell().asSlice();
    return { $$type: 'Context' as const, bounceable: _bounceable, sender: _sender, value: _value, raw: _raw };
}

export function storeTupleContext(source: Context) {
    const builder = new TupleBuilder();
    builder.writeBoolean(source.bounceable);
    builder.writeAddress(source.sender);
    builder.writeNumber(source.value);
    builder.writeSlice(source.raw.asCell());
    return builder.build();
}

export function dictValueParserContext(): DictionaryValue<Context> {
    return {
        serialize: (src, builder) => {
            builder.storeRef(beginCell().store(storeContext(src)).endCell());
        },
        parse: (src) => {
            return loadContext(src.loadRef().beginParse());
        }
    }
}

export type SendParameters = {
    $$type: 'SendParameters';
    mode: bigint;
    body: Cell | null;
    code: Cell | null;
    data: Cell | null;
    value: bigint;
    to: Address;
    bounce: boolean;
}

export function storeSendParameters(src: SendParameters) {
    return (builder: Builder) => {
        const b_0 = builder;
        b_0.storeInt(src.mode, 257);
        if (src.body !== null && src.body !== undefined) { b_0.storeBit(true).storeRef(src.body); } else { b_0.storeBit(false); }
        if (src.code !== null && src.code !== undefined) { b_0.storeBit(true).storeRef(src.code); } else { b_0.storeBit(false); }
        if (src.data !== null && src.data !== undefined) { b_0.storeBit(true).storeRef(src.data); } else { b_0.storeBit(false); }
        b_0.storeInt(src.value, 257);
        b_0.storeAddress(src.to);
        b_0.storeBit(src.bounce);
    };
}

export function loadSendParameters(slice: Slice) {
    const sc_0 = slice;
    const _mode = sc_0.loadIntBig(257);
    const _body = sc_0.loadBit() ? sc_0.loadRef() : null;
    const _code = sc_0.loadBit() ? sc_0.loadRef() : null;
    const _data = sc_0.loadBit() ? sc_0.loadRef() : null;
    const _value = sc_0.loadIntBig(257);
    const _to = sc_0.loadAddress();
    const _bounce = sc_0.loadBit();
    return { $$type: 'SendParameters' as const, mode: _mode, body: _body, code: _code, data: _data, value: _value, to: _to, bounce: _bounce };
}

export function loadTupleSendParameters(source: TupleReader) {
    const _mode = source.readBigNumber();
    const _body = source.readCellOpt();
    const _code = source.readCellOpt();
    const _data = source.readCellOpt();
    const _value = source.readBigNumber();
    const _to = source.readAddress();
    const _bounce = source.readBoolean();
    return { $$type: 'SendParameters' as const, mode: _mode, body: _body, code: _code, data: _data, value: _value, to: _to, bounce: _bounce };
}

export function loadGetterTupleSendParameters(source: TupleReader) {
    const _mode = source.readBigNumber();
    const _body = source.readCellOpt();
    const _code = source.readCellOpt();
    const _data = source.readCellOpt();
    const _value = source.readBigNumber();
    const _to = source.readAddress();
    const _bounce = source.readBoolean();
    return { $$type: 'SendParameters' as const, mode: _mode, body: _body, code: _code, data: _data, value: _value, to: _to, bounce: _bounce };
}

export function storeTupleSendParameters(source: SendParameters) {
    const builder = new TupleBuilder();
    builder.writeNumber(source.mode);
    builder.writeCell(source.body);
    builder.writeCell(source.code);
    builder.writeCell(source.data);
    builder.writeNumber(source.value);
    builder.writeAddress(source.to);
    builder.writeBoolean(source.bounce);
    return builder.build();
}

export function dictValueParserSendParameters(): DictionaryValue<SendParameters> {
    return {
        serialize: (src, builder) => {
            builder.storeRef(beginCell().store(storeSendParameters(src)).endCell());
        },
        parse: (src) => {
            return loadSendParameters(src.loadRef().beginParse());
        }
    }
}

export type MessageParameters = {
    $$type: 'MessageParameters';
    mode: bigint;
    body: Cell | null;
    value: bigint;
    to: Address;
    bounce: boolean;
}

export function storeMessageParameters(src: MessageParameters) {
    return (builder: Builder) => {
        const b_0 = builder;
        b_0.storeInt(src.mode, 257);
        if (src.body !== null && src.body !== undefined) { b_0.storeBit(true).storeRef(src.body); } else { b_0.storeBit(false); }
        b_0.storeInt(src.value, 257);
        b_0.storeAddress(src.to);
        b_0.storeBit(src.bounce);
    };
}

export function loadMessageParameters(slice: Slice) {
    const sc_0 = slice;
    const _mode = sc_0.loadIntBig(257);
    const _body = sc_0.loadBit() ? sc_0.loadRef() : null;
    const _value = sc_0.loadIntBig(257);
    const _to = sc_0.loadAddress();
    const _bounce = sc_0.loadBit();
    return { $$type: 'MessageParameters' as const, mode: _mode, body: _body, value: _value, to: _to, bounce: _bounce };
}

export function loadTupleMessageParameters(source: TupleReader) {
    const _mode = source.readBigNumber();
    const _body = source.readCellOpt();
    const _value = source.readBigNumber();
    const _to = source.readAddress();
    const _bounce = source.readBoolean();
    return { $$type: 'MessageParameters' as const, mode: _mode, body: _body, value: _value, to: _to, bounce: _bounce };
}

export function loadGetterTupleMessageParameters(source: TupleReader) {
    const _mode = source.readBigNumber();
    const _body = source.readCellOpt();
    const _value = source.readBigNumber();
    const _to = source.readAddress();
    const _bounce = source.readBoolean();
    return { $$type: 'MessageParameters' as const, mode: _mode, body: _body, value: _value, to: _to, bounce: _bounce };
}

export function storeTupleMessageParameters(source: MessageParameters) {
    const builder = new TupleBuilder();
    builder.writeNumber(source.mode);
    builder.writeCell(source.body);
    builder.writeNumber(source.value);
    builder.writeAddress(source.to);
    builder.writeBoolean(source.bounce);
    return builder.build();
}

export function dictValueParserMessageParameters(): DictionaryValue<MessageParameters> {
    return {
        serialize: (src, builder) => {
            builder.storeRef(beginCell().store(storeMessageParameters(src)).endCell());
        },
        parse: (src) => {
            return loadMessageParameters(src.loadRef().beginParse());
        }
    }
}

export type DeployParameters = {
    $$type: 'DeployParameters';
    mode: bigint;
    body: Cell | null;
    value: bigint;
    bounce: boolean;
    init: StateInit;
}

export function storeDeployParameters(src: DeployParameters) {
    return (builder: Builder) => {
        const b_0 = builder;
        b_0.storeInt(src.mode, 257);
        if (src.body !== null && src.body !== undefined) { b_0.storeBit(true).storeRef(src.body); } else { b_0.storeBit(false); }
        b_0.storeInt(src.value, 257);
        b_0.storeBit(src.bounce);
        b_0.store(storeStateInit(src.init));
    };
}

export function loadDeployParameters(slice: Slice) {
    const sc_0 = slice;
    const _mode = sc_0.loadIntBig(257);
    const _body = sc_0.loadBit() ? sc_0.loadRef() : null;
    const _value = sc_0.loadIntBig(257);
    const _bounce = sc_0.loadBit();
    const _init = loadStateInit(sc_0);
    return { $$type: 'DeployParameters' as const, mode: _mode, body: _body, value: _value, bounce: _bounce, init: _init };
}

export function loadTupleDeployParameters(source: TupleReader) {
    const _mode = source.readBigNumber();
    const _body = source.readCellOpt();
    const _value = source.readBigNumber();
    const _bounce = source.readBoolean();
    const _init = loadTupleStateInit(source);
    return { $$type: 'DeployParameters' as const, mode: _mode, body: _body, value: _value, bounce: _bounce, init: _init };
}

export function loadGetterTupleDeployParameters(source: TupleReader) {
    const _mode = source.readBigNumber();
    const _body = source.readCellOpt();
    const _value = source.readBigNumber();
    const _bounce = source.readBoolean();
    const _init = loadGetterTupleStateInit(source);
    return { $$type: 'DeployParameters' as const, mode: _mode, body: _body, value: _value, bounce: _bounce, init: _init };
}

export function storeTupleDeployParameters(source: DeployParameters) {
    const builder = new TupleBuilder();
    builder.writeNumber(source.mode);
    builder.writeCell(source.body);
    builder.writeNumber(source.value);
    builder.writeBoolean(source.bounce);
    builder.writeTuple(storeTupleStateInit(source.init));
    return builder.build();
}

export function dictValueParserDeployParameters(): DictionaryValue<DeployParameters> {
    return {
        serialize: (src, builder) => {
            builder.storeRef(beginCell().store(storeDeployParameters(src)).endCell());
        },
        parse: (src) => {
            return loadDeployParameters(src.loadRef().beginParse());
        }
    }
}

export type StdAddress = {
    $$type: 'StdAddress';
    workchain: bigint;
    address: bigint;
}

export function storeStdAddress(src: StdAddress) {
    return (builder: Builder) => {
        const b_0 = builder;
        b_0.storeInt(src.workchain, 8);
        b_0.storeUint(src.address, 256);
    };
}

export function loadStdAddress(slice: Slice) {
    const sc_0 = slice;
    const _workchain = sc_0.loadIntBig(8);
    const _address = sc_0.loadUintBig(256);
    return { $$type: 'StdAddress' as const, workchain: _workchain, address: _address };
}

export function loadTupleStdAddress(source: TupleReader) {
    const _workchain = source.readBigNumber();
    const _address = source.readBigNumber();
    return { $$type: 'StdAddress' as const, workchain: _workchain, address: _address };
}

export function loadGetterTupleStdAddress(source: TupleReader) {
    const _workchain = source.readBigNumber();
    const _address = source.readBigNumber();
    return { $$type: 'StdAddress' as const, workchain: _workchain, address: _address };
}

export function storeTupleStdAddress(source: StdAddress) {
    const builder = new TupleBuilder();
    builder.writeNumber(source.workchain);
    builder.writeNumber(source.address);
    return builder.build();
}

export function dictValueParserStdAddress(): DictionaryValue<StdAddress> {
    return {
        serialize: (src, builder) => {
            builder.storeRef(beginCell().store(storeStdAddress(src)).endCell());
        },
        parse: (src) => {
            return loadStdAddress(src.loadRef().beginParse());
        }
    }
}

export type VarAddress = {
    $$type: 'VarAddress';
    workchain: bigint;
    address: Slice;
}

export function storeVarAddress(src: VarAddress) {
    return (builder: Builder) => {
        const b_0 = builder;
        b_0.storeInt(src.workchain, 32);
        b_0.storeRef(src.address.asCell());
    };
}

export function loadVarAddress(slice: Slice) {
    const sc_0 = slice;
    const _workchain = sc_0.loadIntBig(32);
    const _address = sc_0.loadRef().asSlice();
    return { $$type: 'VarAddress' as const, workchain: _workchain, address: _address };
}

export function loadTupleVarAddress(source: TupleReader) {
    const _workchain = source.readBigNumber();
    const _address = source.readCell().asSlice();
    return { $$type: 'VarAddress' as const, workchain: _workchain, address: _address };
}

export function loadGetterTupleVarAddress(source: TupleReader) {
    const _workchain = source.readBigNumber();
    const _address = source.readCell().asSlice();
    return { $$type: 'VarAddress' as const, workchain: _workchain, address: _address };
}

export function storeTupleVarAddress(source: VarAddress) {
    const builder = new TupleBuilder();
    builder.writeNumber(source.workchain);
    builder.writeSlice(source.address.asCell());
    return builder.build();
}

export function dictValueParserVarAddress(): DictionaryValue<VarAddress> {
    return {
        serialize: (src, builder) => {
            builder.storeRef(beginCell().store(storeVarAddress(src)).endCell());
        },
        parse: (src) => {
            return loadVarAddress(src.loadRef().beginParse());
        }
    }
}

export type BasechainAddress = {
    $$type: 'BasechainAddress';
    hash: bigint | null;
}

export function storeBasechainAddress(src: BasechainAddress) {
    return (builder: Builder) => {
        const b_0 = builder;
        if (src.hash !== null && src.hash !== undefined) { b_0.storeBit(true).storeInt(src.hash, 257); } else { b_0.storeBit(false); }
    };
}

export function loadBasechainAddress(slice: Slice) {
    const sc_0 = slice;
    const _hash = sc_0.loadBit() ? sc_0.loadIntBig(257) : null;
    return { $$type: 'BasechainAddress' as const, hash: _hash };
}

export function loadTupleBasechainAddress(source: TupleReader) {
    const _hash = source.readBigNumberOpt();
    return { $$type: 'BasechainAddress' as const, hash: _hash };
}

export function loadGetterTupleBasechainAddress(source: TupleReader) {
    const _hash = source.readBigNumberOpt();
    return { $$type: 'BasechainAddress' as const, hash: _hash };
}

export function storeTupleBasechainAddress(source: BasechainAddress) {
    const builder = new TupleBuilder();
    builder.writeNumber(source.hash);
    return builder.build();
}

export function dictValueParserBasechainAddress(): DictionaryValue<BasechainAddress> {
    return {
        serialize: (src, builder) => {
            builder.storeRef(beginCell().store(storeBasechainAddress(src)).endCell());
        },
        parse: (src) => {
            return loadBasechainAddress(src.loadRef().beginParse());
        }
    }
}

export type Deploy = {
    $$type: 'Deploy';
    queryId: bigint;
}

export function storeDeploy(src: Deploy) {
    return (builder: Builder) => {
        const b_0 = builder;
        b_0.storeUint(2490013878, 32);
        b_0.storeUint(src.queryId, 64);
    };
}

export function loadDeploy(slice: Slice) {
    const sc_0 = slice;
    if (sc_0.loadUint(32) !== 2490013878) { throw Error('Invalid prefix'); }
    const _queryId = sc_0.loadUintBig(64);
    return { $$type: 'Deploy' as const, queryId: _queryId };
}

export function loadTupleDeploy(source: TupleReader) {
    const _queryId = source.readBigNumber();
    return { $$type: 'Deploy' as const, queryId: _queryId };
}

export function loadGetterTupleDeploy(source: TupleReader) {
    const _queryId = source.readBigNumber();
    return { $$type: 'Deploy' as const, queryId: _queryId };
}

export function storeTupleDeploy(source: Deploy) {
    const builder = new TupleBuilder();
    builder.writeNumber(source.queryId);
    return builder.build();
}

export function dictValueParserDeploy(): DictionaryValue<Deploy> {
    return {
        serialize: (src, builder) => {
            builder.storeRef(beginCell().store(storeDeploy(src)).endCell());
        },
        parse: (src) => {
            return loadDeploy(src.loadRef().beginParse());
        }
    }
}

export type DeployOk = {
    $$type: 'DeployOk';
    queryId: bigint;
}

export function storeDeployOk(src: DeployOk) {
    return (builder: Builder) => {
        const b_0 = builder;
        b_0.storeUint(2952335191, 32);
        b_0.storeUint(src.queryId, 64);
    };
}

export function loadDeployOk(slice: Slice) {
    const sc_0 = slice;
    if (sc_0.loadUint(32) !== 2952335191) { throw Error('Invalid prefix'); }
    const _queryId = sc_0.loadUintBig(64);
    return { $$type: 'DeployOk' as const, queryId: _queryId };
}

export function loadTupleDeployOk(source: TupleReader) {
    const _queryId = source.readBigNumber();
    return { $$type: 'DeployOk' as const, queryId: _queryId };
}

export function loadGetterTupleDeployOk(source: TupleReader) {
    const _queryId = source.readBigNumber();
    return { $$type: 'DeployOk' as const, queryId: _queryId };
}

export function storeTupleDeployOk(source: DeployOk) {
    const builder = new TupleBuilder();
    builder.writeNumber(source.queryId);
    return builder.build();
}

export function dictValueParserDeployOk(): DictionaryValue<DeployOk> {
    return {
        serialize: (src, builder) => {
            builder.storeRef(beginCell().store(storeDeployOk(src)).endCell());
        },
        parse: (src) => {
            return loadDeployOk(src.loadRef().beginParse());
        }
    }
}

export type FactoryDeploy = {
    $$type: 'FactoryDeploy';
    queryId: bigint;
    cashback: Address;
}

export function storeFactoryDeploy(src: FactoryDeploy) {
    return (builder: Builder) => {
        const b_0 = builder;
        b_0.storeUint(1829761339, 32);
        b_0.storeUint(src.queryId, 64);
        b_0.storeAddress(src.cashback);
    };
}

export function loadFactoryDeploy(slice: Slice) {
    const sc_0 = slice;
    if (sc_0.loadUint(32) !== 1829761339) { throw Error('Invalid prefix'); }
    const _queryId = sc_0.loadUintBig(64);
    const _cashback = sc_0.loadAddress();
    return { $$type: 'FactoryDeploy' as const, queryId: _queryId, cashback: _cashback };
}

export function loadTupleFactoryDeploy(source: TupleReader) {
    const _queryId = source.readBigNumber();
    const _cashback = source.readAddress();
    return { $$type: 'FactoryDeploy' as const, queryId: _queryId, cashback: _cashback };
}

export function loadGetterTupleFactoryDeploy(source: TupleReader) {
    const _queryId = source.readBigNumber();
    const _cashback = source.readAddress();
    return { $$type: 'FactoryDeploy' as const, queryId: _queryId, cashback: _cashback };
}

export function storeTupleFactoryDeploy(source: FactoryDeploy) {
    const builder = new TupleBuilder();
    builder.writeNumber(source.queryId);
    builder.writeAddress(source.cashback);
    return builder.build();
}

export function dictValueParserFactoryDeploy(): DictionaryValue<FactoryDeploy> {
    return {
        serialize: (src, builder) => {
            builder.storeRef(beginCell().store(storeFactoryDeploy(src)).endCell());
        },
        parse: (src) => {
            return loadFactoryDeploy(src.loadRef().beginParse());
        }
    }
}

export type ChangeOwner = {
    $$type: 'ChangeOwner';
    queryId: bigint;
    newOwner: Address;
}

export function storeChangeOwner(src: ChangeOwner) {
    return (builder: Builder) => {
        const b_0 = builder;
        b_0.storeUint(2174598809, 32);
        b_0.storeUint(src.queryId, 64);
        b_0.storeAddress(src.newOwner);
    };
}

export function loadChangeOwner(slice: Slice) {
    const sc_0 = slice;
    if (sc_0.loadUint(32) !== 2174598809) { throw Error('Invalid prefix'); }
    const _queryId = sc_0.loadUintBig(64);
    const _newOwner = sc_0.loadAddress();
    return { $$type: 'ChangeOwner' as const, queryId: _queryId, newOwner: _newOwner };
}

export function loadTupleChangeOwner(source: TupleReader) {
    const _queryId = source.readBigNumber();
    const _newOwner = source.readAddress();
    return { $$type: 'ChangeOwner' as const, queryId: _queryId, newOwner: _newOwner };
}

export function loadGetterTupleChangeOwner(source: TupleReader) {
    const _queryId = source.readBigNumber();
    const _newOwner = source.readAddress();
    return { $$type: 'ChangeOwner' as const, queryId: _queryId, newOwner: _newOwner };
}

export function storeTupleChangeOwner(source: ChangeOwner) {
    const builder = new TupleBuilder();
    builder.writeNumber(source.queryId);
    builder.writeAddress(source.newOwner);
    return builder.build();
}

export function dictValueParserChangeOwner(): DictionaryValue<ChangeOwner> {
    return {
        serialize: (src, builder) => {
            builder.storeRef(beginCell().store(storeChangeOwner(src)).endCell());
        },
        parse: (src) => {
            return loadChangeOwner(src.loadRef().beginParse());
        }
    }
}

export type ChangeOwnerOk = {
    $$type: 'ChangeOwnerOk';
    queryId: bigint;
    newOwner: Address;
}

export function storeChangeOwnerOk(src: ChangeOwnerOk) {
    return (builder: Builder) => {
        const b_0 = builder;
        b_0.storeUint(846932810, 32);
        b_0.storeUint(src.queryId, 64);
        b_0.storeAddress(src.newOwner);
    };
}

export function loadChangeOwnerOk(slice: Slice) {
    const sc_0 = slice;
    if (sc_0.loadUint(32) !== 846932810) { throw Error('Invalid prefix'); }
    const _queryId = sc_0.loadUintBig(64);
    const _newOwner = sc_0.loadAddress();
    return { $$type: 'ChangeOwnerOk' as const, queryId: _queryId, newOwner: _newOwner };
}

export function loadTupleChangeOwnerOk(source: TupleReader) {
    const _queryId = source.readBigNumber();
    const _newOwner = source.readAddress();
    return { $$type: 'ChangeOwnerOk' as const, queryId: _queryId, newOwner: _newOwner };
}

export function loadGetterTupleChangeOwnerOk(source: TupleReader) {
    const _queryId = source.readBigNumber();
    const _newOwner = source.readAddress();
    return { $$type: 'ChangeOwnerOk' as const, queryId: _queryId, newOwner: _newOwner };
}

export function storeTupleChangeOwnerOk(source: ChangeOwnerOk) {
    const builder = new TupleBuilder();
    builder.writeNumber(source.queryId);
    builder.writeAddress(source.newOwner);
    return builder.build();
}

export function dictValueParserChangeOwnerOk(): DictionaryValue<ChangeOwnerOk> {
    return {
        serialize: (src, builder) => {
            builder.storeRef(beginCell().store(storeChangeOwnerOk(src)).endCell());
        },
        parse: (src) => {
            return loadChangeOwnerOk(src.loadRef().beginParse());
        }
    }
}

export type CreateOrder = {
    $$type: 'CreateOrder';
    orderId: bigint;
    fiatAmount: bigint;
    fiatCurrency: bigint;
    paymentTimeout: bigint;
}

export function storeCreateOrder(src: CreateOrder) {
    return (builder: Builder) => {
        const b_0 = builder;
        b_0.storeUint(3932749128, 32);
        b_0.storeUint(src.orderId, 64);
        b_0.storeUint(src.fiatAmount, 64);
        b_0.storeUint(src.fiatCurrency, 8);
        b_0.storeUint(src.paymentTimeout, 32);
    };
}

export function loadCreateOrder(slice: Slice) {
    const sc_0 = slice;
    if (sc_0.loadUint(32) !== 3932749128) { throw Error('Invalid prefix'); }
    const _orderId = sc_0.loadUintBig(64);
    const _fiatAmount = sc_0.loadUintBig(64);
    const _fiatCurrency = sc_0.loadUintBig(8);
    const _paymentTimeout = sc_0.loadUintBig(32);
    return { $$type: 'CreateOrder' as const, orderId: _orderId, fiatAmount: _fiatAmount, fiatCurrency: _fiatCurrency, paymentTimeout: _paymentTimeout };
}

export function loadTupleCreateOrder(source: TupleReader) {
    const _orderId = source.readBigNumber();
    const _fiatAmount = source.readBigNumber();
    const _fiatCurrency = source.readBigNumber();
    const _paymentTimeout = source.readBigNumber();
    return { $$type: 'CreateOrder' as const, orderId: _orderId, fiatAmount: _fiatAmount, fiatCurrency: _fiatCurrency, paymentTimeout: _paymentTimeout };
}

export function loadGetterTupleCreateOrder(source: TupleReader) {
    const _orderId = source.readBigNumber();
    const _fiatAmount = source.readBigNumber();
    const _fiatCurrency = source.readBigNumber();
    const _paymentTimeout = source.readBigNumber();
    return { $$type: 'CreateOrder' as const, orderId: _orderId, fiatAmount: _fiatAmount, fiatCurrency: _fiatCurrency, paymentTimeout: _paymentTimeout };
}

export function storeTupleCreateOrder(source: CreateOrder) {
    const builder = new TupleBuilder();
    builder.writeNumber(source.orderId);
    builder.writeNumber(source.fiatAmount);
    builder.writeNumber(source.fiatCurrency);
    builder.writeNumber(source.paymentTimeout);
    return builder.build();
}

export function dictValueParserCreateOrder(): DictionaryValue<CreateOrder> {
    return {
        serialize: (src, builder) => {
            builder.storeRef(beginCell().store(storeCreateOrder(src)).endCell());
        },
        parse: (src) => {
            return loadCreateOrder(src.loadRef().beginParse());
        }
    }
}

export type TakeOrder = {
    $$type: 'TakeOrder';
    orderId: bigint;
}

export function storeTakeOrder(src: TakeOrder) {
    return (builder: Builder) => {
        const b_0 = builder;
        b_0.storeUint(3808834696, 32);
        b_0.storeUint(src.orderId, 64);
    };
}

export function loadTakeOrder(slice: Slice) {
    const sc_0 = slice;
    if (sc_0.loadUint(32) !== 3808834696) { throw Error('Invalid prefix'); }
    const _orderId = sc_0.loadUintBig(64);
    return { $$type: 'TakeOrder' as const, orderId: _orderId };
}

export function loadTupleTakeOrder(source: TupleReader) {
    const _orderId = source.readBigNumber();
    return { $$type: 'TakeOrder' as const, orderId: _orderId };
}

export function loadGetterTupleTakeOrder(source: TupleReader) {
    const _orderId = source.readBigNumber();
    return { $$type: 'TakeOrder' as const, orderId: _orderId };
}

export function storeTupleTakeOrder(source: TakeOrder) {
    const builder = new TupleBuilder();
    builder.writeNumber(source.orderId);
    return builder.build();
}

export function dictValueParserTakeOrder(): DictionaryValue<TakeOrder> {
    return {
        serialize: (src, builder) => {
            builder.storeRef(beginCell().store(storeTakeOrder(src)).endCell());
        },
        parse: (src) => {
            return loadTakeOrder(src.loadRef().beginParse());
        }
    }
}

export type ConfirmPayment = {
    $$type: 'ConfirmPayment';
    orderId: bigint;
}

export function storeConfirmPayment(src: ConfirmPayment) {
    return (builder: Builder) => {
        const b_0 = builder;
        b_0.storeUint(4285838865, 32);
        b_0.storeUint(src.orderId, 64);
    };
}

export function loadConfirmPayment(slice: Slice) {
    const sc_0 = slice;
    if (sc_0.loadUint(32) !== 4285838865) { throw Error('Invalid prefix'); }
    const _orderId = sc_0.loadUintBig(64);
    return { $$type: 'ConfirmPayment' as const, orderId: _orderId };
}

export function loadTupleConfirmPayment(source: TupleReader) {
    const _orderId = source.readBigNumber();
    return { $$type: 'ConfirmPayment' as const, orderId: _orderId };
}

export function loadGetterTupleConfirmPayment(source: TupleReader) {
    const _orderId = source.readBigNumber();
    return { $$type: 'ConfirmPayment' as const, orderId: _orderId };
}

export function storeTupleConfirmPayment(source: ConfirmPayment) {
    const builder = new TupleBuilder();
    builder.writeNumber(source.orderId);
    return builder.build();
}

export function dictValueParserConfirmPayment(): DictionaryValue<ConfirmPayment> {
    return {
        serialize: (src, builder) => {
            builder.storeRef(beginCell().store(storeConfirmPayment(src)).endCell());
        },
        parse: (src) => {
            return loadConfirmPayment(src.loadRef().beginParse());
        }
    }
}

export type OracleConfirm = {
    $$type: 'OracleConfirm';
    orderId: bigint;
    signature: Slice;
}

export function storeOracleConfirm(src: OracleConfirm) {
    return (builder: Builder) => {
        const b_0 = builder;
        b_0.storeUint(2418055224, 32);
        b_0.storeUint(src.orderId, 64);
        b_0.storeRef(src.signature.asCell());
    };
}

export function loadOracleConfirm(slice: Slice) {
    const sc_0 = slice;
    if (sc_0.loadUint(32) !== 2418055224) { throw Error('Invalid prefix'); }
    const _orderId = sc_0.loadUintBig(64);
    const _signature = sc_0.loadRef().asSlice();
    return { $$type: 'OracleConfirm' as const, orderId: _orderId, signature: _signature };
}

export function loadTupleOracleConfirm(source: TupleReader) {
    const _orderId = source.readBigNumber();
    const _signature = source.readCell().asSlice();
    return { $$type: 'OracleConfirm' as const, orderId: _orderId, signature: _signature };
}

export function loadGetterTupleOracleConfirm(source: TupleReader) {
    const _orderId = source.readBigNumber();
    const _signature = source.readCell().asSlice();
    return { $$type: 'OracleConfirm' as const, orderId: _orderId, signature: _signature };
}

export function storeTupleOracleConfirm(source: OracleConfirm) {
    const builder = new TupleBuilder();
    builder.writeNumber(source.orderId);
    builder.writeSlice(source.signature.asCell());
    return builder.build();
}

export function dictValueParserOracleConfirm(): DictionaryValue<OracleConfirm> {
    return {
        serialize: (src, builder) => {
            builder.storeRef(beginCell().store(storeOracleConfirm(src)).endCell());
        },
        parse: (src) => {
            return loadOracleConfirm(src.loadRef().beginParse());
        }
    }
}

export type CancelOrder = {
    $$type: 'CancelOrder';
    orderId: bigint;
}

export function storeCancelOrder(src: CancelOrder) {
    return (builder: Builder) => {
        const b_0 = builder;
        b_0.storeUint(3037552710, 32);
        b_0.storeUint(src.orderId, 64);
    };
}

export function loadCancelOrder(slice: Slice) {
    const sc_0 = slice;
    if (sc_0.loadUint(32) !== 3037552710) { throw Error('Invalid prefix'); }
    const _orderId = sc_0.loadUintBig(64);
    return { $$type: 'CancelOrder' as const, orderId: _orderId };
}

export function loadTupleCancelOrder(source: TupleReader) {
    const _orderId = source.readBigNumber();
    return { $$type: 'CancelOrder' as const, orderId: _orderId };
}

export function loadGetterTupleCancelOrder(source: TupleReader) {
    const _orderId = source.readBigNumber();
    return { $$type: 'CancelOrder' as const, orderId: _orderId };
}

export function storeTupleCancelOrder(source: CancelOrder) {
    const builder = new TupleBuilder();
    builder.writeNumber(source.orderId);
    return builder.build();
}

export function dictValueParserCancelOrder(): DictionaryValue<CancelOrder> {
    return {
        serialize: (src, builder) => {
            builder.storeRef(beginCell().store(storeCancelOrder(src)).endCell());
        },
        parse: (src) => {
            return loadCancelOrder(src.loadRef().beginParse());
        }
    }
}

export type OpenDispute = {
    $$type: 'OpenDispute';
    orderId: bigint;
    reason: bigint;
}

export function storeOpenDispute(src: OpenDispute) {
    return (builder: Builder) => {
        const b_0 = builder;
        b_0.storeUint(2721478341, 32);
        b_0.storeUint(src.orderId, 64);
        b_0.storeUint(src.reason, 8);
    };
}

export function loadOpenDispute(slice: Slice) {
    const sc_0 = slice;
    if (sc_0.loadUint(32) !== 2721478341) { throw Error('Invalid prefix'); }
    const _orderId = sc_0.loadUintBig(64);
    const _reason = sc_0.loadUintBig(8);
    return { $$type: 'OpenDispute' as const, orderId: _orderId, reason: _reason };
}

export function loadTupleOpenDispute(source: TupleReader) {
    const _orderId = source.readBigNumber();
    const _reason = source.readBigNumber();
    return { $$type: 'OpenDispute' as const, orderId: _orderId, reason: _reason };
}

export function loadGetterTupleOpenDispute(source: TupleReader) {
    const _orderId = source.readBigNumber();
    const _reason = source.readBigNumber();
    return { $$type: 'OpenDispute' as const, orderId: _orderId, reason: _reason };
}

export function storeTupleOpenDispute(source: OpenDispute) {
    const builder = new TupleBuilder();
    builder.writeNumber(source.orderId);
    builder.writeNumber(source.reason);
    return builder.build();
}

export function dictValueParserOpenDispute(): DictionaryValue<OpenDispute> {
    return {
        serialize: (src, builder) => {
            builder.storeRef(beginCell().store(storeOpenDispute(src)).endCell());
        },
        parse: (src) => {
            return loadOpenDispute(src.loadRef().beginParse());
        }
    }
}

export type ResolveDispute = {
    $$type: 'ResolveDispute';
    orderId: bigint;
    winner: Address;
}

export function storeResolveDispute(src: ResolveDispute) {
    return (builder: Builder) => {
        const b_0 = builder;
        b_0.storeUint(2928221909, 32);
        b_0.storeUint(src.orderId, 64);
        b_0.storeAddress(src.winner);
    };
}

export function loadResolveDispute(slice: Slice) {
    const sc_0 = slice;
    if (sc_0.loadUint(32) !== 2928221909) { throw Error('Invalid prefix'); }
    const _orderId = sc_0.loadUintBig(64);
    const _winner = sc_0.loadAddress();
    return { $$type: 'ResolveDispute' as const, orderId: _orderId, winner: _winner };
}

export function loadTupleResolveDispute(source: TupleReader) {
    const _orderId = source.readBigNumber();
    const _winner = source.readAddress();
    return { $$type: 'ResolveDispute' as const, orderId: _orderId, winner: _winner };
}

export function loadGetterTupleResolveDispute(source: TupleReader) {
    const _orderId = source.readBigNumber();
    const _winner = source.readAddress();
    return { $$type: 'ResolveDispute' as const, orderId: _orderId, winner: _winner };
}

export function storeTupleResolveDispute(source: ResolveDispute) {
    const builder = new TupleBuilder();
    builder.writeNumber(source.orderId);
    builder.writeAddress(source.winner);
    return builder.build();
}

export function dictValueParserResolveDispute(): DictionaryValue<ResolveDispute> {
    return {
        serialize: (src, builder) => {
            builder.storeRef(beginCell().store(storeResolveDispute(src)).endCell());
        },
        parse: (src) => {
            return loadResolveDispute(src.loadRef().beginParse());
        }
    }
}

export type WithdrawFees = {
    $$type: 'WithdrawFees';
    amount: bigint;
}

export function storeWithdrawFees(src: WithdrawFees) {
    return (builder: Builder) => {
        const b_0 = builder;
        b_0.storeUint(3947541004, 32);
        b_0.storeCoins(src.amount);
    };
}

export function loadWithdrawFees(slice: Slice) {
    const sc_0 = slice;
    if (sc_0.loadUint(32) !== 3947541004) { throw Error('Invalid prefix'); }
    const _amount = sc_0.loadCoins();
    return { $$type: 'WithdrawFees' as const, amount: _amount };
}

export function loadTupleWithdrawFees(source: TupleReader) {
    const _amount = source.readBigNumber();
    return { $$type: 'WithdrawFees' as const, amount: _amount };
}

export function loadGetterTupleWithdrawFees(source: TupleReader) {
    const _amount = source.readBigNumber();
    return { $$type: 'WithdrawFees' as const, amount: _amount };
}

export function storeTupleWithdrawFees(source: WithdrawFees) {
    const builder = new TupleBuilder();
    builder.writeNumber(source.amount);
    return builder.build();
}

export function dictValueParserWithdrawFees(): DictionaryValue<WithdrawFees> {
    return {
        serialize: (src, builder) => {
            builder.storeRef(beginCell().store(storeWithdrawFees(src)).endCell());
        },
        parse: (src) => {
            return loadWithdrawFees(src.loadRef().beginParse());
        }
    }
}

export type UpdateOracle = {
    $$type: 'UpdateOracle';
    newOracle: Address;
}

export function storeUpdateOracle(src: UpdateOracle) {
    return (builder: Builder) => {
        const b_0 = builder;
        b_0.storeUint(1084263761, 32);
        b_0.storeAddress(src.newOracle);
    };
}

export function loadUpdateOracle(slice: Slice) {
    const sc_0 = slice;
    if (sc_0.loadUint(32) !== 1084263761) { throw Error('Invalid prefix'); }
    const _newOracle = sc_0.loadAddress();
    return { $$type: 'UpdateOracle' as const, newOracle: _newOracle };
}

export function loadTupleUpdateOracle(source: TupleReader) {
    const _newOracle = source.readAddress();
    return { $$type: 'UpdateOracle' as const, newOracle: _newOracle };
}

export function loadGetterTupleUpdateOracle(source: TupleReader) {
    const _newOracle = source.readAddress();
    return { $$type: 'UpdateOracle' as const, newOracle: _newOracle };
}

export function storeTupleUpdateOracle(source: UpdateOracle) {
    const builder = new TupleBuilder();
    builder.writeAddress(source.newOracle);
    return builder.build();
}

export function dictValueParserUpdateOracle(): DictionaryValue<UpdateOracle> {
    return {
        serialize: (src, builder) => {
            builder.storeRef(beginCell().store(storeUpdateOracle(src)).endCell());
        },
        parse: (src) => {
            return loadUpdateOracle(src.loadRef().beginParse());
        }
    }
}

export type UpdateFee = {
    $$type: 'UpdateFee';
    newFeePercent: bigint;
}

export function storeUpdateFee(src: UpdateFee) {
    return (builder: Builder) => {
        const b_0 = builder;
        b_0.storeUint(2988846933, 32);
        b_0.storeUint(src.newFeePercent, 16);
    };
}

export function loadUpdateFee(slice: Slice) {
    const sc_0 = slice;
    if (sc_0.loadUint(32) !== 2988846933) { throw Error('Invalid prefix'); }
    const _newFeePercent = sc_0.loadUintBig(16);
    return { $$type: 'UpdateFee' as const, newFeePercent: _newFeePercent };
}

export function loadTupleUpdateFee(source: TupleReader) {
    const _newFeePercent = source.readBigNumber();
    return { $$type: 'UpdateFee' as const, newFeePercent: _newFeePercent };
}

export function loadGetterTupleUpdateFee(source: TupleReader) {
    const _newFeePercent = source.readBigNumber();
    return { $$type: 'UpdateFee' as const, newFeePercent: _newFeePercent };
}

export function storeTupleUpdateFee(source: UpdateFee) {
    const builder = new TupleBuilder();
    builder.writeNumber(source.newFeePercent);
    return builder.build();
}

export function dictValueParserUpdateFee(): DictionaryValue<UpdateFee> {
    return {
        serialize: (src, builder) => {
            builder.storeRef(beginCell().store(storeUpdateFee(src)).endCell());
        },
        parse: (src) => {
            return loadUpdateFee(src.loadRef().beginParse());
        }
    }
}

export type Order = {
    $$type: 'Order';
    seller: Address;
    buyer: Address | null;
    cryptoAmount: bigint;
    fiatAmount: bigint;
    fiatCurrency: bigint;
    status: bigint;
    createdAt: bigint;
    lockedAt: bigint;
    paymentTimeout: bigint;
}

export function storeOrder(src: Order) {
    return (builder: Builder) => {
        const b_0 = builder;
        b_0.storeAddress(src.seller);
        b_0.storeAddress(src.buyer);
        b_0.storeCoins(src.cryptoAmount);
        b_0.storeUint(src.fiatAmount, 64);
        b_0.storeUint(src.fiatCurrency, 8);
        b_0.storeUint(src.status, 8);
        b_0.storeUint(src.createdAt, 32);
        b_0.storeUint(src.lockedAt, 32);
        b_0.storeUint(src.paymentTimeout, 32);
    };
}

export function loadOrder(slice: Slice) {
    const sc_0 = slice;
    const _seller = sc_0.loadAddress();
    const _buyer = sc_0.loadMaybeAddress();
    const _cryptoAmount = sc_0.loadCoins();
    const _fiatAmount = sc_0.loadUintBig(64);
    const _fiatCurrency = sc_0.loadUintBig(8);
    const _status = sc_0.loadUintBig(8);
    const _createdAt = sc_0.loadUintBig(32);
    const _lockedAt = sc_0.loadUintBig(32);
    const _paymentTimeout = sc_0.loadUintBig(32);
    return { $$type: 'Order' as const, seller: _seller, buyer: _buyer, cryptoAmount: _cryptoAmount, fiatAmount: _fiatAmount, fiatCurrency: _fiatCurrency, status: _status, createdAt: _createdAt, lockedAt: _lockedAt, paymentTimeout: _paymentTimeout };
}

export function loadTupleOrder(source: TupleReader) {
    const _seller = source.readAddress();
    const _buyer = source.readAddressOpt();
    const _cryptoAmount = source.readBigNumber();
    const _fiatAmount = source.readBigNumber();
    const _fiatCurrency = source.readBigNumber();
    const _status = source.readBigNumber();
    const _createdAt = source.readBigNumber();
    const _lockedAt = source.readBigNumber();
    const _paymentTimeout = source.readBigNumber();
    return { $$type: 'Order' as const, seller: _seller, buyer: _buyer, cryptoAmount: _cryptoAmount, fiatAmount: _fiatAmount, fiatCurrency: _fiatCurrency, status: _status, createdAt: _createdAt, lockedAt: _lockedAt, paymentTimeout: _paymentTimeout };
}

export function loadGetterTupleOrder(source: TupleReader) {
    const _seller = source.readAddress();
    const _buyer = source.readAddressOpt();
    const _cryptoAmount = source.readBigNumber();
    const _fiatAmount = source.readBigNumber();
    const _fiatCurrency = source.readBigNumber();
    const _status = source.readBigNumber();
    const _createdAt = source.readBigNumber();
    const _lockedAt = source.readBigNumber();
    const _paymentTimeout = source.readBigNumber();
    return { $$type: 'Order' as const, seller: _seller, buyer: _buyer, cryptoAmount: _cryptoAmount, fiatAmount: _fiatAmount, fiatCurrency: _fiatCurrency, status: _status, createdAt: _createdAt, lockedAt: _lockedAt, paymentTimeout: _paymentTimeout };
}

export function storeTupleOrder(source: Order) {
    const builder = new TupleBuilder();
    builder.writeAddress(source.seller);
    builder.writeAddress(source.buyer);
    builder.writeNumber(source.cryptoAmount);
    builder.writeNumber(source.fiatAmount);
    builder.writeNumber(source.fiatCurrency);
    builder.writeNumber(source.status);
    builder.writeNumber(source.createdAt);
    builder.writeNumber(source.lockedAt);
    builder.writeNumber(source.paymentTimeout);
    return builder.build();
}

export function dictValueParserOrder(): DictionaryValue<Order> {
    return {
        serialize: (src, builder) => {
            builder.storeRef(beginCell().store(storeOrder(src)).endCell());
        },
        parse: (src) => {
            return loadOrder(src.loadRef().beginParse());
        }
    }
}

export type P2PEscrow$Data = {
    $$type: 'P2PEscrow$Data';
    owner: Address;
    oracle: Address;
    feePercent: bigint;
    collectedFees: bigint;
    orderCounter: bigint;
    orders: Dictionary<bigint, Order>;
}

export function storeP2PEscrow$Data(src: P2PEscrow$Data) {
    return (builder: Builder) => {
        const b_0 = builder;
        b_0.storeAddress(src.owner);
        b_0.storeAddress(src.oracle);
        b_0.storeUint(src.feePercent, 16);
        b_0.storeCoins(src.collectedFees);
        b_0.storeUint(src.orderCounter, 64);
        b_0.storeDict(src.orders, Dictionary.Keys.BigInt(257), dictValueParserOrder());
    };
}

export function loadP2PEscrow$Data(slice: Slice) {
    const sc_0 = slice;
    const _owner = sc_0.loadAddress();
    const _oracle = sc_0.loadAddress();
    const _feePercent = sc_0.loadUintBig(16);
    const _collectedFees = sc_0.loadCoins();
    const _orderCounter = sc_0.loadUintBig(64);
    const _orders = Dictionary.load(Dictionary.Keys.BigInt(257), dictValueParserOrder(), sc_0);
    return { $$type: 'P2PEscrow$Data' as const, owner: _owner, oracle: _oracle, feePercent: _feePercent, collectedFees: _collectedFees, orderCounter: _orderCounter, orders: _orders };
}

export function loadTupleP2PEscrow$Data(source: TupleReader) {
    const _owner = source.readAddress();
    const _oracle = source.readAddress();
    const _feePercent = source.readBigNumber();
    const _collectedFees = source.readBigNumber();
    const _orderCounter = source.readBigNumber();
    const _orders = Dictionary.loadDirect(Dictionary.Keys.BigInt(257), dictValueParserOrder(), source.readCellOpt());
    return { $$type: 'P2PEscrow$Data' as const, owner: _owner, oracle: _oracle, feePercent: _feePercent, collectedFees: _collectedFees, orderCounter: _orderCounter, orders: _orders };
}

export function loadGetterTupleP2PEscrow$Data(source: TupleReader) {
    const _owner = source.readAddress();
    const _oracle = source.readAddress();
    const _feePercent = source.readBigNumber();
    const _collectedFees = source.readBigNumber();
    const _orderCounter = source.readBigNumber();
    const _orders = Dictionary.loadDirect(Dictionary.Keys.BigInt(257), dictValueParserOrder(), source.readCellOpt());
    return { $$type: 'P2PEscrow$Data' as const, owner: _owner, oracle: _oracle, feePercent: _feePercent, collectedFees: _collectedFees, orderCounter: _orderCounter, orders: _orders };
}

export function storeTupleP2PEscrow$Data(source: P2PEscrow$Data) {
    const builder = new TupleBuilder();
    builder.writeAddress(source.owner);
    builder.writeAddress(source.oracle);
    builder.writeNumber(source.feePercent);
    builder.writeNumber(source.collectedFees);
    builder.writeNumber(source.orderCounter);
    builder.writeCell(source.orders.size > 0 ? beginCell().storeDictDirect(source.orders, Dictionary.Keys.BigInt(257), dictValueParserOrder()).endCell() : null);
    return builder.build();
}

export function dictValueParserP2PEscrow$Data(): DictionaryValue<P2PEscrow$Data> {
    return {
        serialize: (src, builder) => {
            builder.storeRef(beginCell().store(storeP2PEscrow$Data(src)).endCell());
        },
        parse: (src) => {
            return loadP2PEscrow$Data(src.loadRef().beginParse());
        }
    }
}

export type OrderCreated = {
    $$type: 'OrderCreated';
    orderId: bigint;
    seller: Address;
    cryptoAmount: bigint;
    fiatAmount: bigint;
}

export function storeOrderCreated(src: OrderCreated) {
    return (builder: Builder) => {
        const b_0 = builder;
        b_0.storeUint(3924115121, 32);
        b_0.storeUint(src.orderId, 64);
        b_0.storeAddress(src.seller);
        b_0.storeCoins(src.cryptoAmount);
        b_0.storeUint(src.fiatAmount, 64);
    };
}

export function loadOrderCreated(slice: Slice) {
    const sc_0 = slice;
    if (sc_0.loadUint(32) !== 3924115121) { throw Error('Invalid prefix'); }
    const _orderId = sc_0.loadUintBig(64);
    const _seller = sc_0.loadAddress();
    const _cryptoAmount = sc_0.loadCoins();
    const _fiatAmount = sc_0.loadUintBig(64);
    return { $$type: 'OrderCreated' as const, orderId: _orderId, seller: _seller, cryptoAmount: _cryptoAmount, fiatAmount: _fiatAmount };
}

export function loadTupleOrderCreated(source: TupleReader) {
    const _orderId = source.readBigNumber();
    const _seller = source.readAddress();
    const _cryptoAmount = source.readBigNumber();
    const _fiatAmount = source.readBigNumber();
    return { $$type: 'OrderCreated' as const, orderId: _orderId, seller: _seller, cryptoAmount: _cryptoAmount, fiatAmount: _fiatAmount };
}

export function loadGetterTupleOrderCreated(source: TupleReader) {
    const _orderId = source.readBigNumber();
    const _seller = source.readAddress();
    const _cryptoAmount = source.readBigNumber();
    const _fiatAmount = source.readBigNumber();
    return { $$type: 'OrderCreated' as const, orderId: _orderId, seller: _seller, cryptoAmount: _cryptoAmount, fiatAmount: _fiatAmount };
}

export function storeTupleOrderCreated(source: OrderCreated) {
    const builder = new TupleBuilder();
    builder.writeNumber(source.orderId);
    builder.writeAddress(source.seller);
    builder.writeNumber(source.cryptoAmount);
    builder.writeNumber(source.fiatAmount);
    return builder.build();
}

export function dictValueParserOrderCreated(): DictionaryValue<OrderCreated> {
    return {
        serialize: (src, builder) => {
            builder.storeRef(beginCell().store(storeOrderCreated(src)).endCell());
        },
        parse: (src) => {
            return loadOrderCreated(src.loadRef().beginParse());
        }
    }
}

export type OrderTaken = {
    $$type: 'OrderTaken';
    orderId: bigint;
    buyer: Address;
}

export function storeOrderTaken(src: OrderTaken) {
    return (builder: Builder) => {
        const b_0 = builder;
        b_0.storeUint(2146179729, 32);
        b_0.storeUint(src.orderId, 64);
        b_0.storeAddress(src.buyer);
    };
}

export function loadOrderTaken(slice: Slice) {
    const sc_0 = slice;
    if (sc_0.loadUint(32) !== 2146179729) { throw Error('Invalid prefix'); }
    const _orderId = sc_0.loadUintBig(64);
    const _buyer = sc_0.loadAddress();
    return { $$type: 'OrderTaken' as const, orderId: _orderId, buyer: _buyer };
}

export function loadTupleOrderTaken(source: TupleReader) {
    const _orderId = source.readBigNumber();
    const _buyer = source.readAddress();
    return { $$type: 'OrderTaken' as const, orderId: _orderId, buyer: _buyer };
}

export function loadGetterTupleOrderTaken(source: TupleReader) {
    const _orderId = source.readBigNumber();
    const _buyer = source.readAddress();
    return { $$type: 'OrderTaken' as const, orderId: _orderId, buyer: _buyer };
}

export function storeTupleOrderTaken(source: OrderTaken) {
    const builder = new TupleBuilder();
    builder.writeNumber(source.orderId);
    builder.writeAddress(source.buyer);
    return builder.build();
}

export function dictValueParserOrderTaken(): DictionaryValue<OrderTaken> {
    return {
        serialize: (src, builder) => {
            builder.storeRef(beginCell().store(storeOrderTaken(src)).endCell());
        },
        parse: (src) => {
            return loadOrderTaken(src.loadRef().beginParse());
        }
    }
}

export type OrderCompleted = {
    $$type: 'OrderCompleted';
    orderId: bigint;
    buyer: Address;
    amount: bigint;
}

export function storeOrderCompleted(src: OrderCompleted) {
    return (builder: Builder) => {
        const b_0 = builder;
        b_0.storeUint(742725614, 32);
        b_0.storeUint(src.orderId, 64);
        b_0.storeAddress(src.buyer);
        b_0.storeCoins(src.amount);
    };
}

export function loadOrderCompleted(slice: Slice) {
    const sc_0 = slice;
    if (sc_0.loadUint(32) !== 742725614) { throw Error('Invalid prefix'); }
    const _orderId = sc_0.loadUintBig(64);
    const _buyer = sc_0.loadAddress();
    const _amount = sc_0.loadCoins();
    return { $$type: 'OrderCompleted' as const, orderId: _orderId, buyer: _buyer, amount: _amount };
}

export function loadTupleOrderCompleted(source: TupleReader) {
    const _orderId = source.readBigNumber();
    const _buyer = source.readAddress();
    const _amount = source.readBigNumber();
    return { $$type: 'OrderCompleted' as const, orderId: _orderId, buyer: _buyer, amount: _amount };
}

export function loadGetterTupleOrderCompleted(source: TupleReader) {
    const _orderId = source.readBigNumber();
    const _buyer = source.readAddress();
    const _amount = source.readBigNumber();
    return { $$type: 'OrderCompleted' as const, orderId: _orderId, buyer: _buyer, amount: _amount };
}

export function storeTupleOrderCompleted(source: OrderCompleted) {
    const builder = new TupleBuilder();
    builder.writeNumber(source.orderId);
    builder.writeAddress(source.buyer);
    builder.writeNumber(source.amount);
    return builder.build();
}

export function dictValueParserOrderCompleted(): DictionaryValue<OrderCompleted> {
    return {
        serialize: (src, builder) => {
            builder.storeRef(beginCell().store(storeOrderCompleted(src)).endCell());
        },
        parse: (src) => {
            return loadOrderCompleted(src.loadRef().beginParse());
        }
    }
}

export type OrderCancelled = {
    $$type: 'OrderCancelled';
    orderId: bigint;
}

export function storeOrderCancelled(src: OrderCancelled) {
    return (builder: Builder) => {
        const b_0 = builder;
        b_0.storeUint(2918286519, 32);
        b_0.storeUint(src.orderId, 64);
    };
}

export function loadOrderCancelled(slice: Slice) {
    const sc_0 = slice;
    if (sc_0.loadUint(32) !== 2918286519) { throw Error('Invalid prefix'); }
    const _orderId = sc_0.loadUintBig(64);
    return { $$type: 'OrderCancelled' as const, orderId: _orderId };
}

export function loadTupleOrderCancelled(source: TupleReader) {
    const _orderId = source.readBigNumber();
    return { $$type: 'OrderCancelled' as const, orderId: _orderId };
}

export function loadGetterTupleOrderCancelled(source: TupleReader) {
    const _orderId = source.readBigNumber();
    return { $$type: 'OrderCancelled' as const, orderId: _orderId };
}

export function storeTupleOrderCancelled(source: OrderCancelled) {
    const builder = new TupleBuilder();
    builder.writeNumber(source.orderId);
    return builder.build();
}

export function dictValueParserOrderCancelled(): DictionaryValue<OrderCancelled> {
    return {
        serialize: (src, builder) => {
            builder.storeRef(beginCell().store(storeOrderCancelled(src)).endCell());
        },
        parse: (src) => {
            return loadOrderCancelled(src.loadRef().beginParse());
        }
    }
}

export type DisputeOpened = {
    $$type: 'DisputeOpened';
    orderId: bigint;
    openedBy: Address;
}

export function storeDisputeOpened(src: DisputeOpened) {
    return (builder: Builder) => {
        const b_0 = builder;
        b_0.storeUint(842545338, 32);
        b_0.storeUint(src.orderId, 64);
        b_0.storeAddress(src.openedBy);
    };
}

export function loadDisputeOpened(slice: Slice) {
    const sc_0 = slice;
    if (sc_0.loadUint(32) !== 842545338) { throw Error('Invalid prefix'); }
    const _orderId = sc_0.loadUintBig(64);
    const _openedBy = sc_0.loadAddress();
    return { $$type: 'DisputeOpened' as const, orderId: _orderId, openedBy: _openedBy };
}

export function loadTupleDisputeOpened(source: TupleReader) {
    const _orderId = source.readBigNumber();
    const _openedBy = source.readAddress();
    return { $$type: 'DisputeOpened' as const, orderId: _orderId, openedBy: _openedBy };
}

export function loadGetterTupleDisputeOpened(source: TupleReader) {
    const _orderId = source.readBigNumber();
    const _openedBy = source.readAddress();
    return { $$type: 'DisputeOpened' as const, orderId: _orderId, openedBy: _openedBy };
}

export function storeTupleDisputeOpened(source: DisputeOpened) {
    const builder = new TupleBuilder();
    builder.writeNumber(source.orderId);
    builder.writeAddress(source.openedBy);
    return builder.build();
}

export function dictValueParserDisputeOpened(): DictionaryValue<DisputeOpened> {
    return {
        serialize: (src, builder) => {
            builder.storeRef(beginCell().store(storeDisputeOpened(src)).endCell());
        },
        parse: (src) => {
            return loadDisputeOpened(src.loadRef().beginParse());
        }
    }
}

export type DisputeResolved = {
    $$type: 'DisputeResolved';
    orderId: bigint;
    winner: Address;
}

export function storeDisputeResolved(src: DisputeResolved) {
    return (builder: Builder) => {
        const b_0 = builder;
        b_0.storeUint(52015006, 32);
        b_0.storeUint(src.orderId, 64);
        b_0.storeAddress(src.winner);
    };
}

export function loadDisputeResolved(slice: Slice) {
    const sc_0 = slice;
    if (sc_0.loadUint(32) !== 52015006) { throw Error('Invalid prefix'); }
    const _orderId = sc_0.loadUintBig(64);
    const _winner = sc_0.loadAddress();
    return { $$type: 'DisputeResolved' as const, orderId: _orderId, winner: _winner };
}

export function loadTupleDisputeResolved(source: TupleReader) {
    const _orderId = source.readBigNumber();
    const _winner = source.readAddress();
    return { $$type: 'DisputeResolved' as const, orderId: _orderId, winner: _winner };
}

export function loadGetterTupleDisputeResolved(source: TupleReader) {
    const _orderId = source.readBigNumber();
    const _winner = source.readAddress();
    return { $$type: 'DisputeResolved' as const, orderId: _orderId, winner: _winner };
}

export function storeTupleDisputeResolved(source: DisputeResolved) {
    const builder = new TupleBuilder();
    builder.writeNumber(source.orderId);
    builder.writeAddress(source.winner);
    return builder.build();
}

export function dictValueParserDisputeResolved(): DictionaryValue<DisputeResolved> {
    return {
        serialize: (src, builder) => {
            builder.storeRef(beginCell().store(storeDisputeResolved(src)).endCell());
        },
        parse: (src) => {
            return loadDisputeResolved(src.loadRef().beginParse());
        }
    }
}

 type P2PEscrow_init_args = {
    $$type: 'P2PEscrow_init_args';
    oracle: Address;
}

function initP2PEscrow_init_args(src: P2PEscrow_init_args) {
    return (builder: Builder) => {
        const b_0 = builder;
        b_0.storeAddress(src.oracle);
    };
}

async function P2PEscrow_init(oracle: Address) {
    const __code = Cell.fromBoc(Buffer.from('b5ee9c7241023101000bdf000228ff008e88f4a413f4bcf2c80bed5320e303ed43d90112020271020702015803050163b4a3bda89a1a400031c21f481f481a61ff401a67fe808aaa0d82d1c21f4800203a2dbf0850064e040206aaa07c5b678d8c30040002250163b659fda89a1a400031c21f481f481a61ff401a67fe808aaa0d82d1c21f4800203a2dbf0850064e040206aaa07c5b678d8c3006000221020120081002016a090b0163ac1076a268690000c7087d207d206987fd00699ffa022aa8360b47087d200080e8b6fc2140193810081aaa81f16d9e3630c00a0002240201480c0e0161a6bdda89a1a400031c21f481f481a61ff401a67fe808aaa0d82d1c21f4800203a2dbf0850064e040206aaa07c5b678d8c30d0002220161a48dda89a1a400031c21f481f481a61ff401a67fe808aaa0d82d1c21f4800203a2dbf0850064e040206aaa07c5b678d8c30f0002230193bb50ded44d0d200018e10fa40fa40d30ffa00d33ff40455506c168e10fa400101d16df8428032702010355503e25505db3c6c61206e92306d99206ef2d0806f296f09e2206e92306dde8110070810101220259f40d6fa192306ddf206e92306d8e22d0fa40d72c01916d93fa4001e201fa00d33fd307d307d31fd31fd31f55806c196f09e203f630eda2edfb01d072d721d200d200fa4021103450666f04f86102f862ed44d0d200018e10fa40fa40d30ffa00d33ff40455506c168e10fa400101d16df8428032702010355503e207925f07e025d749c21fe30005f90182f016d6ebdf693a7d12679060fcc66c2c70a4e2b79aa42d7a697c6dc605410fbebbbae302132f30045205d31f218210ea68fd48bae302218210e3063488bae302218210ff74b611bae30221821090209838ba1416181901ec31d33fd33fd307d31f30f8416f243032811dc422821005f5e100bef2f46d70f8235313516005516a05104a4a3309080706050443138101015029c855805089ce5006206e9430cf84809201cee25004fa0212cb3fcb07cb07cb1fcb1fcb1fc925103c01206e953059f45a30944133f415e208a44919c815009455308210e9e53eb15005cb1f13cb3fce01fa02cb3fc9c88258c000000000000000000000000101cb67ccc970fb005513c87f01ca0055505056ce13cecb0f01fa02cb3ff400c9ed54db3101f831d33f30f8416f2410235f03278101012359f40d6fa192306ddf206e92306d8e22d0fa40d72c01916d93fa4001e201fa00d33fd307d307d31fd31fd31f55806c196f09e2814856216eb3f2f4206ef2d0806f2931368200e02702c00012f2f48127755367c705b3f2f471f823284317080706050443138101015029c81700fa55805089ce5006206e9430cf84809201cee25004fa0212cb3fcb07cb07cb1fcb1fcb1fc923103a01206e953059f45a30944133f415e207c85982107fec1a915003cb1fcb3fcec9c88258c000000000000000000000000101cb67ccc970fb0010355512c87f01ca0055505056ce13cecb0f01fa02cb3ff400c9ed54db3102f831d33f30f8416f2410235f03278101012359f40d6fa192306ddf206e92306d8e22d0fa40d72c01916d93fa4001e201fa00d33fd307d307d31fd31fd31f55806c196f09e2814856216eb3f2f4206ef2d0806f29288200d8ec0bc7051af2f48200a07223c001917f9323c002e2f2f410df10ce10bd10ac10ab5507db3c1a2403fe8f7531d33f30f8416f245b8165203225c705f2f4268101012259f40d6fa192306ddf206e92306d8e22d0fa40d72c01916d93fa4001e201fa00d33fd307d307d31fd31fd31f55806c196f09e2814856216eb3f2f4206ef2d0806f298200a07224c001917f9324c002e2f2f410df10ce10bd10ac10abdb3ce0218210b50d60461a241d02f6338200acd6276eb3f2f4535ca8812710a9045360a150dda01068732807106a05104a4a30080706050443138101015029c855805089ce5006206e9430cf84809201cee25004fa0212cb3fcb07cb07cb1fcb1fcb1fc924103601206e953059f45a30944133f415e223206ef2d080728828595a6d6d40037fc8cf85801b1c002c0000000050325020457363726f772052656c6561736500c2ca00cf8440ce01fa028069cf40025c6e016eb0935bcf819d58cf8680cf8480f400f400cf81e2f400c901fb0003206ef2d0805005c8552082102c4517ee5004cb1f12cb3fce01fa02c9c88258c000000000000000000000000101cb67ccc970fb00043ebae302218210a23676c5bae302218210ae891ed5bae302218210eb4ab20cba1e22252a01fc31d33f30f8416f2410235f03278101012359f40d6fa192306ddf206e92306d8e22d0fa40d72c01916d93fa4001e201fa00d33fd307d307d31fd31fd31f55806c196f09e2814856216eb3f2f4206ef2d0806f29288200a0e20bc7051af2f4811fbe03c00013f2f47527075160060543440a080706050443138101015029c81f02e655805089ce5006206e9430cf84809201cee25004fa0212cb3fcb07cb07cb1fcb1fcb1fc924103b01206e953059f45a30944133f415e27288103a102a5a6d6d40037fc8cf8580ca00cf8440ce01fa028069cf40025c6e016eb0935bcf819d58cf8680cf8480f400f400cf81e2f400c901fb00c82021002a0000000050325020457363726f7720526566756e640086018210adf184b758cb1fcb3fc9c88258c000000000000000000000000101cb67ccc970fb0010355512c87f01ca0055505056ce13cecb0f01fa02cb3ff400c9ed54db3101fe31d33f30f8416f2410235f03278101012359f40d6fa192306ddf206e92306d8e22d0fa40d72c01916d93fa4001e201fa00d33fd307d307d31fd31fd31f55806c196f09e2814856216eb3f2f4206ef2d0806f298200ed4d539ac705917f9b538a216e925b7092c705e2e2f2f481697a24c00192347f9304c002e214f2f474032301e0080706050443138101015029c855805089ce5006206e9430cf84809201cee25004fa0212cb3fcb07cb07cb1fcb1fcb1fc923103a01206e953059f45a30944133f415e207c8598210323838ba5003cb1fcb3fcec9c88258c000000000000000000000000101cb67ccc970fb0010355512240034c87f01ca0055505056ce13cecb0f01fa02cb3ff400c9ed54db3101fe31d33ffa4030f8416f245b8200f4ae3227c705f2f4278101012359f40d6fa192306ddf206e92306d8e22d0fa40d72c01916d93fa4001e201fa00d33fd307d307d31fd31fd31f55806c196f09e2814856216eb3f2f4206ef2d0806f298200f26e04c00414f2f48200b5a45398c705917f9b5397206e925b7092c705e2e2f2f42603fa10671057732806054334080706050443138101015029c855805089ce5006206e9430cf84809201cee25004fa0212cb3fcb07cb07cb1fcb1fcb1fc924103b01206e953059f45a30944133f415e2728823034bbb5a6d6d40037fc8cf8580ca00cf8440ce01fa028069cf40025c6e016eb0935bcf818ae2f400c901fb00c82728290034000000005032502044697370757465205265736f6c7574696f6e001a58cf8680cf8480f400f400cf81008859820b19af9e5003cb1fcb3fcec9c88258c000000000000000000000000101cb67ccc970fb0010355512c87f01ca0055505056ce13cecb0f01fa02cb3ff400c9ed54db3103fc8f7331fa0030104510344136db3c8153c95373bbf2f45126a17288270349995a6d6d40037fc8cf8580ca00cf8440ce01fa028069cf40025c6e016eb0935bcf819d58cf8680cf8480f400f400cf81e2f400c901fb00104510344300c87f01ca0055505056ce13cecb0f01fa02cb3ff400c9ed54db31e021821040a08d51ba2d2b2c002e000000005032502046656573205769746864726177616c03f68eab31fa4030104510344136db3c3410455502c87f01ca0055505056ce13cecb0f01fa02cb3ff400c9ed54db31e0218210b2262f55ba8eb731d30f30104510344136db3c338200b438268101f4bbf2f41045103458c87f01ca0055505056ce13cecb0f01fa02cb3ff400c9ed54db31e0018210946a98b6bae302052d2d2e0010f84226c705f2e08400aad33f30c8018210aff90f5758cb1fcb3fc910461035443012f84270705003804201503304c8cf8580ca00cf8440ce01fa02806acf40f400c901fb00c87f01ca0055505056ce13cecb0f01fa02cb3ff400c9ed54db31003810355512c87f01ca0055505056ce13cecb0f01fa02cb3ff400c9ed54000a5f06f2c0820ca0a989', 'hex'))[0];
    const builder = beginCell();
    builder.storeUint(0, 1);
    initP2PEscrow_init_args({ $$type: 'P2PEscrow_init_args', oracle })(builder);
    const __data = builder.endCell();
    return { code: __code, data: __data };
}

export const P2PEscrow_errors = {
    2: { message: "Stack underflow" },
    3: { message: "Stack overflow" },
    4: { message: "Integer overflow" },
    5: { message: "Integer out of expected range" },
    6: { message: "Invalid opcode" },
    7: { message: "Type check error" },
    8: { message: "Cell overflow" },
    9: { message: "Cell underflow" },
    10: { message: "Dictionary error" },
    11: { message: "'Unknown' error" },
    12: { message: "Fatal error" },
    13: { message: "Out of gas error" },
    14: { message: "Virtualization error" },
    32: { message: "Action list is invalid" },
    33: { message: "Action list is too long" },
    34: { message: "Action is invalid or not supported" },
    35: { message: "Invalid source address in outbound message" },
    36: { message: "Invalid destination address in outbound message" },
    37: { message: "Not enough Toncoin" },
    38: { message: "Not enough extra currencies" },
    39: { message: "Outbound message does not fit into a cell after rewriting" },
    40: { message: "Cannot process a message" },
    41: { message: "Library reference is null" },
    42: { message: "Library change action error" },
    43: { message: "Exceeded maximum number of cells in the library or the maximum depth of the Merkle tree" },
    50: { message: "Account state size exceeded limits" },
    128: { message: "Null reference exception" },
    129: { message: "Invalid serialization prefix" },
    130: { message: "Invalid incoming message" },
    131: { message: "Constraints error" },
    132: { message: "Access denied" },
    133: { message: "Contract stopped" },
    134: { message: "Invalid argument" },
    135: { message: "Code of a contract was not found" },
    136: { message: "Invalid standard address" },
    138: { message: "Not a basechain address" },
    7620: { message: "Deposit too small" },
    8126: { message: "Can only cancel open orders" },
    10101: { message: "Cannot buy own order" },
    18518: { message: "Order not found" },
    21449: { message: "Not enough fees" },
    25888: { message: "Only oracle can confirm" },
    27002: { message: "Invalid status" },
    41074: { message: "Order not locked" },
    41186: { message: "Only seller can cancel" },
    44246: { message: "No buyer" },
    46136: { message: "Fee too high" },
    46500: { message: "Invalid winner" },
    55532: { message: "Only seller can confirm" },
    57383: { message: "Order not available" },
    60749: { message: "Not a party" },
    62062: { message: "Not disputed" },
    62638: { message: "Only owner can resolve" },
} as const

export const P2PEscrow_errors_backward = {
    "Stack underflow": 2,
    "Stack overflow": 3,
    "Integer overflow": 4,
    "Integer out of expected range": 5,
    "Invalid opcode": 6,
    "Type check error": 7,
    "Cell overflow": 8,
    "Cell underflow": 9,
    "Dictionary error": 10,
    "'Unknown' error": 11,
    "Fatal error": 12,
    "Out of gas error": 13,
    "Virtualization error": 14,
    "Action list is invalid": 32,
    "Action list is too long": 33,
    "Action is invalid or not supported": 34,
    "Invalid source address in outbound message": 35,
    "Invalid destination address in outbound message": 36,
    "Not enough Toncoin": 37,
    "Not enough extra currencies": 38,
    "Outbound message does not fit into a cell after rewriting": 39,
    "Cannot process a message": 40,
    "Library reference is null": 41,
    "Library change action error": 42,
    "Exceeded maximum number of cells in the library or the maximum depth of the Merkle tree": 43,
    "Account state size exceeded limits": 50,
    "Null reference exception": 128,
    "Invalid serialization prefix": 129,
    "Invalid incoming message": 130,
    "Constraints error": 131,
    "Access denied": 132,
    "Contract stopped": 133,
    "Invalid argument": 134,
    "Code of a contract was not found": 135,
    "Invalid standard address": 136,
    "Not a basechain address": 138,
    "Deposit too small": 7620,
    "Can only cancel open orders": 8126,
    "Cannot buy own order": 10101,
    "Order not found": 18518,
    "Not enough fees": 21449,
    "Only oracle can confirm": 25888,
    "Invalid status": 27002,
    "Order not locked": 41074,
    "Only seller can cancel": 41186,
    "No buyer": 44246,
    "Fee too high": 46136,
    "Invalid winner": 46500,
    "Only seller can confirm": 55532,
    "Order not available": 57383,
    "Not a party": 60749,
    "Not disputed": 62062,
    "Only owner can resolve": 62638,
} as const

const P2PEscrow_types: ABIType[] = [
    {"name":"DataSize","header":null,"fields":[{"name":"cells","type":{"kind":"simple","type":"int","optional":false,"format":257}},{"name":"bits","type":{"kind":"simple","type":"int","optional":false,"format":257}},{"name":"refs","type":{"kind":"simple","type":"int","optional":false,"format":257}}]},
    {"name":"SignedBundle","header":null,"fields":[{"name":"signature","type":{"kind":"simple","type":"fixed-bytes","optional":false,"format":64}},{"name":"signedData","type":{"kind":"simple","type":"slice","optional":false,"format":"remainder"}}]},
    {"name":"StateInit","header":null,"fields":[{"name":"code","type":{"kind":"simple","type":"cell","optional":false}},{"name":"data","type":{"kind":"simple","type":"cell","optional":false}}]},
    {"name":"Context","header":null,"fields":[{"name":"bounceable","type":{"kind":"simple","type":"bool","optional":false}},{"name":"sender","type":{"kind":"simple","type":"address","optional":false}},{"name":"value","type":{"kind":"simple","type":"int","optional":false,"format":257}},{"name":"raw","type":{"kind":"simple","type":"slice","optional":false}}]},
    {"name":"SendParameters","header":null,"fields":[{"name":"mode","type":{"kind":"simple","type":"int","optional":false,"format":257}},{"name":"body","type":{"kind":"simple","type":"cell","optional":true}},{"name":"code","type":{"kind":"simple","type":"cell","optional":true}},{"name":"data","type":{"kind":"simple","type":"cell","optional":true}},{"name":"value","type":{"kind":"simple","type":"int","optional":false,"format":257}},{"name":"to","type":{"kind":"simple","type":"address","optional":false}},{"name":"bounce","type":{"kind":"simple","type":"bool","optional":false}}]},
    {"name":"MessageParameters","header":null,"fields":[{"name":"mode","type":{"kind":"simple","type":"int","optional":false,"format":257}},{"name":"body","type":{"kind":"simple","type":"cell","optional":true}},{"name":"value","type":{"kind":"simple","type":"int","optional":false,"format":257}},{"name":"to","type":{"kind":"simple","type":"address","optional":false}},{"name":"bounce","type":{"kind":"simple","type":"bool","optional":false}}]},
    {"name":"DeployParameters","header":null,"fields":[{"name":"mode","type":{"kind":"simple","type":"int","optional":false,"format":257}},{"name":"body","type":{"kind":"simple","type":"cell","optional":true}},{"name":"value","type":{"kind":"simple","type":"int","optional":false,"format":257}},{"name":"bounce","type":{"kind":"simple","type":"bool","optional":false}},{"name":"init","type":{"kind":"simple","type":"StateInit","optional":false}}]},
    {"name":"StdAddress","header":null,"fields":[{"name":"workchain","type":{"kind":"simple","type":"int","optional":false,"format":8}},{"name":"address","type":{"kind":"simple","type":"uint","optional":false,"format":256}}]},
    {"name":"VarAddress","header":null,"fields":[{"name":"workchain","type":{"kind":"simple","type":"int","optional":false,"format":32}},{"name":"address","type":{"kind":"simple","type":"slice","optional":false}}]},
    {"name":"BasechainAddress","header":null,"fields":[{"name":"hash","type":{"kind":"simple","type":"int","optional":true,"format":257}}]},
    {"name":"Deploy","header":2490013878,"fields":[{"name":"queryId","type":{"kind":"simple","type":"uint","optional":false,"format":64}}]},
    {"name":"DeployOk","header":2952335191,"fields":[{"name":"queryId","type":{"kind":"simple","type":"uint","optional":false,"format":64}}]},
    {"name":"FactoryDeploy","header":1829761339,"fields":[{"name":"queryId","type":{"kind":"simple","type":"uint","optional":false,"format":64}},{"name":"cashback","type":{"kind":"simple","type":"address","optional":false}}]},
    {"name":"ChangeOwner","header":2174598809,"fields":[{"name":"queryId","type":{"kind":"simple","type":"uint","optional":false,"format":64}},{"name":"newOwner","type":{"kind":"simple","type":"address","optional":false}}]},
    {"name":"ChangeOwnerOk","header":846932810,"fields":[{"name":"queryId","type":{"kind":"simple","type":"uint","optional":false,"format":64}},{"name":"newOwner","type":{"kind":"simple","type":"address","optional":false}}]},
    {"name":"CreateOrder","header":3932749128,"fields":[{"name":"orderId","type":{"kind":"simple","type":"uint","optional":false,"format":64}},{"name":"fiatAmount","type":{"kind":"simple","type":"uint","optional":false,"format":64}},{"name":"fiatCurrency","type":{"kind":"simple","type":"uint","optional":false,"format":8}},{"name":"paymentTimeout","type":{"kind":"simple","type":"uint","optional":false,"format":32}}]},
    {"name":"TakeOrder","header":3808834696,"fields":[{"name":"orderId","type":{"kind":"simple","type":"uint","optional":false,"format":64}}]},
    {"name":"ConfirmPayment","header":4285838865,"fields":[{"name":"orderId","type":{"kind":"simple","type":"uint","optional":false,"format":64}}]},
    {"name":"OracleConfirm","header":2418055224,"fields":[{"name":"orderId","type":{"kind":"simple","type":"uint","optional":false,"format":64}},{"name":"signature","type":{"kind":"simple","type":"slice","optional":false}}]},
    {"name":"CancelOrder","header":3037552710,"fields":[{"name":"orderId","type":{"kind":"simple","type":"uint","optional":false,"format":64}}]},
    {"name":"OpenDispute","header":2721478341,"fields":[{"name":"orderId","type":{"kind":"simple","type":"uint","optional":false,"format":64}},{"name":"reason","type":{"kind":"simple","type":"uint","optional":false,"format":8}}]},
    {"name":"ResolveDispute","header":2928221909,"fields":[{"name":"orderId","type":{"kind":"simple","type":"uint","optional":false,"format":64}},{"name":"winner","type":{"kind":"simple","type":"address","optional":false}}]},
    {"name":"WithdrawFees","header":3947541004,"fields":[{"name":"amount","type":{"kind":"simple","type":"uint","optional":false,"format":"coins"}}]},
    {"name":"UpdateOracle","header":1084263761,"fields":[{"name":"newOracle","type":{"kind":"simple","type":"address","optional":false}}]},
    {"name":"UpdateFee","header":2988846933,"fields":[{"name":"newFeePercent","type":{"kind":"simple","type":"uint","optional":false,"format":16}}]},
    {"name":"Order","header":null,"fields":[{"name":"seller","type":{"kind":"simple","type":"address","optional":false}},{"name":"buyer","type":{"kind":"simple","type":"address","optional":true}},{"name":"cryptoAmount","type":{"kind":"simple","type":"uint","optional":false,"format":"coins"}},{"name":"fiatAmount","type":{"kind":"simple","type":"uint","optional":false,"format":64}},{"name":"fiatCurrency","type":{"kind":"simple","type":"uint","optional":false,"format":8}},{"name":"status","type":{"kind":"simple","type":"uint","optional":false,"format":8}},{"name":"createdAt","type":{"kind":"simple","type":"uint","optional":false,"format":32}},{"name":"lockedAt","type":{"kind":"simple","type":"uint","optional":false,"format":32}},{"name":"paymentTimeout","type":{"kind":"simple","type":"uint","optional":false,"format":32}}]},
    {"name":"P2PEscrow$Data","header":null,"fields":[{"name":"owner","type":{"kind":"simple","type":"address","optional":false}},{"name":"oracle","type":{"kind":"simple","type":"address","optional":false}},{"name":"feePercent","type":{"kind":"simple","type":"uint","optional":false,"format":16}},{"name":"collectedFees","type":{"kind":"simple","type":"uint","optional":false,"format":"coins"}},{"name":"orderCounter","type":{"kind":"simple","type":"uint","optional":false,"format":64}},{"name":"orders","type":{"kind":"dict","key":"int","value":"Order","valueFormat":"ref"}}]},
    {"name":"OrderCreated","header":3924115121,"fields":[{"name":"orderId","type":{"kind":"simple","type":"uint","optional":false,"format":64}},{"name":"seller","type":{"kind":"simple","type":"address","optional":false}},{"name":"cryptoAmount","type":{"kind":"simple","type":"uint","optional":false,"format":"coins"}},{"name":"fiatAmount","type":{"kind":"simple","type":"uint","optional":false,"format":64}}]},
    {"name":"OrderTaken","header":2146179729,"fields":[{"name":"orderId","type":{"kind":"simple","type":"uint","optional":false,"format":64}},{"name":"buyer","type":{"kind":"simple","type":"address","optional":false}}]},
    {"name":"OrderCompleted","header":742725614,"fields":[{"name":"orderId","type":{"kind":"simple","type":"uint","optional":false,"format":64}},{"name":"buyer","type":{"kind":"simple","type":"address","optional":false}},{"name":"amount","type":{"kind":"simple","type":"uint","optional":false,"format":"coins"}}]},
    {"name":"OrderCancelled","header":2918286519,"fields":[{"name":"orderId","type":{"kind":"simple","type":"uint","optional":false,"format":64}}]},
    {"name":"DisputeOpened","header":842545338,"fields":[{"name":"orderId","type":{"kind":"simple","type":"uint","optional":false,"format":64}},{"name":"openedBy","type":{"kind":"simple","type":"address","optional":false}}]},
    {"name":"DisputeResolved","header":52015006,"fields":[{"name":"orderId","type":{"kind":"simple","type":"uint","optional":false,"format":64}},{"name":"winner","type":{"kind":"simple","type":"address","optional":false}}]},
]

const P2PEscrow_opcodes = {
    "Deploy": 2490013878,
    "DeployOk": 2952335191,
    "FactoryDeploy": 1829761339,
    "ChangeOwner": 2174598809,
    "ChangeOwnerOk": 846932810,
    "CreateOrder": 3932749128,
    "TakeOrder": 3808834696,
    "ConfirmPayment": 4285838865,
    "OracleConfirm": 2418055224,
    "CancelOrder": 3037552710,
    "OpenDispute": 2721478341,
    "ResolveDispute": 2928221909,
    "WithdrawFees": 3947541004,
    "UpdateOracle": 1084263761,
    "UpdateFee": 2988846933,
    "OrderCreated": 3924115121,
    "OrderTaken": 2146179729,
    "OrderCompleted": 742725614,
    "OrderCancelled": 2918286519,
    "DisputeOpened": 842545338,
    "DisputeResolved": 52015006,
}

const P2PEscrow_getters: ABIGetter[] = [
    {"name":"getOrder","methodId":128269,"arguments":[{"name":"orderId","type":{"kind":"simple","type":"int","optional":false,"format":257}}],"returnType":{"kind":"simple","type":"Order","optional":true}},
    {"name":"getOrderCount","methodId":94927,"arguments":[],"returnType":{"kind":"simple","type":"int","optional":false,"format":257}},
    {"name":"getCollectedFees","methodId":108894,"arguments":[],"returnType":{"kind":"simple","type":"int","optional":false,"format":257}},
    {"name":"getFeePercent","methodId":109126,"arguments":[],"returnType":{"kind":"simple","type":"int","optional":false,"format":257}},
    {"name":"getOracle","methodId":106528,"arguments":[],"returnType":{"kind":"simple","type":"address","optional":false}},
    {"name":"owner","methodId":83229,"arguments":[],"returnType":{"kind":"simple","type":"address","optional":false}},
]

export const P2PEscrow_getterMapping: { [key: string]: string } = {
    'getOrder': 'getGetOrder',
    'getOrderCount': 'getGetOrderCount',
    'getCollectedFees': 'getGetCollectedFees',
    'getFeePercent': 'getGetFeePercent',
    'getOracle': 'getGetOracle',
    'owner': 'getOwner',
}

const P2PEscrow_receivers: ABIReceiver[] = [
    {"receiver":"internal","message":{"kind":"typed","type":"CreateOrder"}},
    {"receiver":"internal","message":{"kind":"typed","type":"TakeOrder"}},
    {"receiver":"internal","message":{"kind":"typed","type":"ConfirmPayment"}},
    {"receiver":"internal","message":{"kind":"typed","type":"OracleConfirm"}},
    {"receiver":"internal","message":{"kind":"typed","type":"CancelOrder"}},
    {"receiver":"internal","message":{"kind":"text","text":"expire"}},
    {"receiver":"internal","message":{"kind":"typed","type":"OpenDispute"}},
    {"receiver":"internal","message":{"kind":"typed","type":"ResolveDispute"}},
    {"receiver":"internal","message":{"kind":"typed","type":"WithdrawFees"}},
    {"receiver":"internal","message":{"kind":"typed","type":"UpdateOracle"}},
    {"receiver":"internal","message":{"kind":"typed","type":"UpdateFee"}},
    {"receiver":"internal","message":{"kind":"typed","type":"Deploy"}},
]

export const STATUS_OPEN = 0n;
export const STATUS_LOCKED = 1n;
export const STATUS_FIAT_SENT = 2n;
export const STATUS_COMPLETED = 3n;
export const STATUS_DISPUTED = 4n;
export const STATUS_CANCELLED = 5n;
export const STATUS_EXPIRED = 6n;

export class P2PEscrow implements Contract {
    
    public static readonly storageReserve = 0n;
    public static readonly errors = P2PEscrow_errors_backward;
    public static readonly opcodes = P2PEscrow_opcodes;
    
    static async init(oracle: Address) {
        return await P2PEscrow_init(oracle);
    }
    
    static async fromInit(oracle: Address) {
        const __gen_init = await P2PEscrow_init(oracle);
        const address = contractAddress(0, __gen_init);
        return new P2PEscrow(address, __gen_init);
    }
    
    static fromAddress(address: Address) {
        return new P2PEscrow(address);
    }
    
    readonly address: Address; 
    readonly init?: { code: Cell, data: Cell };
    readonly abi: ContractABI = {
        types:  P2PEscrow_types,
        getters: P2PEscrow_getters,
        receivers: P2PEscrow_receivers,
        errors: P2PEscrow_errors,
    };
    
    constructor(address: Address, init?: { code: Cell, data: Cell }) {
        this.address = address;
        this.init = init;
    }
    
    async send(provider: ContractProvider, via: Sender, args: { value: bigint, bounce?: boolean| null | undefined }, message: CreateOrder | TakeOrder | ConfirmPayment | OracleConfirm | CancelOrder | "expire" | OpenDispute | ResolveDispute | WithdrawFees | UpdateOracle | UpdateFee | Deploy) {
        
        let body: Cell | null = null;
        if (message && typeof message === 'object' && !(message instanceof Slice) && message.$$type === 'CreateOrder') {
            body = beginCell().store(storeCreateOrder(message)).endCell();
        }
        if (message && typeof message === 'object' && !(message instanceof Slice) && message.$$type === 'TakeOrder') {
            body = beginCell().store(storeTakeOrder(message)).endCell();
        }
        if (message && typeof message === 'object' && !(message instanceof Slice) && message.$$type === 'ConfirmPayment') {
            body = beginCell().store(storeConfirmPayment(message)).endCell();
        }
        if (message && typeof message === 'object' && !(message instanceof Slice) && message.$$type === 'OracleConfirm') {
            body = beginCell().store(storeOracleConfirm(message)).endCell();
        }
        if (message && typeof message === 'object' && !(message instanceof Slice) && message.$$type === 'CancelOrder') {
            body = beginCell().store(storeCancelOrder(message)).endCell();
        }
        if (message === "expire") {
            body = beginCell().storeUint(0, 32).storeStringTail(message).endCell();
        }
        if (message && typeof message === 'object' && !(message instanceof Slice) && message.$$type === 'OpenDispute') {
            body = beginCell().store(storeOpenDispute(message)).endCell();
        }
        if (message && typeof message === 'object' && !(message instanceof Slice) && message.$$type === 'ResolveDispute') {
            body = beginCell().store(storeResolveDispute(message)).endCell();
        }
        if (message && typeof message === 'object' && !(message instanceof Slice) && message.$$type === 'WithdrawFees') {
            body = beginCell().store(storeWithdrawFees(message)).endCell();
        }
        if (message && typeof message === 'object' && !(message instanceof Slice) && message.$$type === 'UpdateOracle') {
            body = beginCell().store(storeUpdateOracle(message)).endCell();
        }
        if (message && typeof message === 'object' && !(message instanceof Slice) && message.$$type === 'UpdateFee') {
            body = beginCell().store(storeUpdateFee(message)).endCell();
        }
        if (message && typeof message === 'object' && !(message instanceof Slice) && message.$$type === 'Deploy') {
            body = beginCell().store(storeDeploy(message)).endCell();
        }
        if (body === null) { throw new Error('Invalid message type'); }
        
        await provider.internal(via, { ...args, body: body });
        
    }
    
    async getGetOrder(provider: ContractProvider, orderId: bigint) {
        const builder = new TupleBuilder();
        builder.writeNumber(orderId);
        const source = (await provider.get('getOrder', builder.build())).stack;
        const result_p = source.readTupleOpt();
        const result = result_p ? loadTupleOrder(result_p) : null;
        return result;
    }
    
    async getGetOrderCount(provider: ContractProvider) {
        const builder = new TupleBuilder();
        const source = (await provider.get('getOrderCount', builder.build())).stack;
        const result = source.readBigNumber();
        return result;
    }
    
    async getGetCollectedFees(provider: ContractProvider) {
        const builder = new TupleBuilder();
        const source = (await provider.get('getCollectedFees', builder.build())).stack;
        const result = source.readBigNumber();
        return result;
    }
    
    async getGetFeePercent(provider: ContractProvider) {
        const builder = new TupleBuilder();
        const source = (await provider.get('getFeePercent', builder.build())).stack;
        const result = source.readBigNumber();
        return result;
    }
    
    async getGetOracle(provider: ContractProvider) {
        const builder = new TupleBuilder();
        const source = (await provider.get('getOracle', builder.build())).stack;
        const result = source.readAddress();
        return result;
    }
    
    async getOwner(provider: ContractProvider) {
        const builder = new TupleBuilder();
        const source = (await provider.get('owner', builder.build())).stack;
        const result = source.readAddress();
        return result;
    }
    
}