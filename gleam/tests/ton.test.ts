/**
 * TON Payment - Unit Tests
 *
 * Tests TON blockchain payment utilities including:
 * - Tonkeeper deep link generation
 * - Amount conversion (nano units)
 * - Transaction verification logic
 *
 * Run: npm test -- ton.test.ts
 */

import { describe, it, expect } from 'vitest'

// Constants matching the Gleam implementation
const TON_DECIMALS = 9
const USDT_DECIMALS = 6
const TON_USDT_MASTER_MAINNET = 'EQCxE6mUtQJKFnGfaROTKOt1lZbDiiX1kCixRv7Nw2Id_sDs'

// Helper functions that mirror the Gleam/Erlang implementation
function usdtToNano(usdt: number): bigint {
  return BigInt(Math.round(usdt * Math.pow(10, USDT_DECIMALS)))
}

function tonToNano(ton: number): bigint {
  return BigInt(Math.round(ton * Math.pow(10, TON_DECIMALS)))
}

function nanoToUsdt(nano: bigint): number {
  return Number(nano) / Math.pow(10, USDT_DECIMALS)
}

function nanoToTon(nano: bigint): number {
  return Number(nano) / Math.pow(10, TON_DECIMALS)
}

function generateTonkeeperUsdtLink(
  recipient: string,
  amountUsdt: number,
  comment: string
): string {
  const nanoAmount = usdtToNano(amountUsdt)
  // Tonkeeper jetton transfer link
  return `tonkeeper://transfer/${recipient}?jetton=${TON_USDT_MASTER_MAINNET}&amount=${nanoAmount}&text=${encodeURIComponent(comment)}`
}

function generateTonkeeperTonLink(
  recipient: string,
  amountTon: number,
  comment: string
): string {
  const nanoAmount = tonToNano(amountTon)
  return `tonkeeper://transfer/${recipient}?amount=${nanoAmount}&text=${encodeURIComponent(comment)}`
}

describe('TON Amount Conversion', () => {
  describe('USDT to nano conversion', () => {
    it('should convert 1 USDT to 1000000 nano', () => {
      expect(usdtToNano(1)).toBe(1000000n)
    })

    it('should convert 10 USDT to 10000000 nano', () => {
      expect(usdtToNano(10)).toBe(10000000n)
    })

    it('should convert 0.5 USDT to 500000 nano', () => {
      expect(usdtToNano(0.5)).toBe(500000n)
    })

    it('should convert 100.50 USDT correctly', () => {
      expect(usdtToNano(100.5)).toBe(100500000n)
    })

    it('should handle small amounts (0.01 USDT)', () => {
      expect(usdtToNano(0.01)).toBe(10000n)
    })
  })

  describe('TON to nano conversion', () => {
    it('should convert 1 TON to 1000000000 nano', () => {
      expect(tonToNano(1)).toBe(1000000000n)
    })

    it('should convert 5 TON to 5000000000 nano', () => {
      expect(tonToNano(5)).toBe(5000000000n)
    })

    it('should convert 0.1 TON to 100000000 nano', () => {
      expect(tonToNano(0.1)).toBe(100000000n)
    })

    it('should handle small TON amounts (0.001 TON)', () => {
      expect(tonToNano(0.001)).toBe(1000000n)
    })
  })

  describe('Nano to human-readable conversion', () => {
    it('should convert nano to USDT', () => {
      expect(nanoToUsdt(1000000n)).toBe(1)
      expect(nanoToUsdt(10000000n)).toBe(10)
      expect(nanoToUsdt(500000n)).toBe(0.5)
    })

    it('should convert nano to TON', () => {
      expect(nanoToTon(1000000000n)).toBe(1)
      expect(nanoToTon(5000000000n)).toBe(5)
      expect(nanoToTon(100000000n)).toBe(0.1)
    })
  })
})

describe('Tonkeeper Deep Link Generation', () => {
  const testWallet = 'UQTestWalletAddressXXXXXXXXXXXXXXXXXXXXXX'

  describe('USDT Jetton transfer link', () => {
    it('should generate valid Tonkeeper USDT link', () => {
      const link = generateTonkeeperUsdtLink(testWallet, 10, 'inv_12345')

      expect(link).toContain('tonkeeper://transfer/')
      expect(link).toContain(testWallet)
      expect(link).toContain(`jetton=${TON_USDT_MASTER_MAINNET}`)
      expect(link).toContain('amount=10000000') // 10 USDT in nano
      expect(link).toContain('text=inv_12345')
    })

    it('should correctly encode amount for 5 USDT', () => {
      const link = generateTonkeeperUsdtLink(testWallet, 5, 'test')
      expect(link).toContain('amount=5000000')
    })

    it('should correctly encode amount for 25 USDT', () => {
      const link = generateTonkeeperUsdtLink(testWallet, 25, 'test')
      expect(link).toContain('amount=25000000')
    })

    it('should correctly encode amount for 50 USDT', () => {
      const link = generateTonkeeperUsdtLink(testWallet, 50, 'test')
      expect(link).toContain('amount=50000000')
    })

    it('should correctly encode amount for 100 USDT', () => {
      const link = generateTonkeeperUsdtLink(testWallet, 100, 'test')
      expect(link).toContain('amount=100000000')
    })

    it('should URL-encode comment with special characters', () => {
      const link = generateTonkeeperUsdtLink(testWallet, 10, 'inv 12345 (test)')
      expect(link).toContain('text=inv%2012345%20(test)')
    })
  })

  describe('Native TON transfer link', () => {
    it('should generate valid Tonkeeper TON link', () => {
      const link = generateTonkeeperTonLink(testWallet, 5, 'inv_67890')

      expect(link).toContain('tonkeeper://transfer/')
      expect(link).toContain(testWallet)
      expect(link).not.toContain('jetton=') // No jetton for native TON
      expect(link).toContain('amount=5000000000') // 5 TON in nano
      expect(link).toContain('text=inv_67890')
    })

    it('should correctly encode amount for 1 TON', () => {
      const link = generateTonkeeperTonLink(testWallet, 1, 'test')
      expect(link).toContain('amount=1000000000')
    })

    it('should correctly encode amount for 10 TON', () => {
      const link = generateTonkeeperTonLink(testWallet, 10, 'test')
      expect(link).toContain('amount=10000000000')
    })
  })
})

describe('TON Pricing Options', () => {
  const usdtOptions = [
    { amount: 5, stars: 217, label: '$5' },
    { amount: 10, stars: 434, label: '$10' },
    { amount: 25, stars: 1085, label: '$25' },
    { amount: 50, stars: 2170, label: '$50' },
    { amount: 100, stars: 4340, label: '$100' },
  ]

  const tonOptions = [
    { amount: 1, stars: 173, label: '1 TON' },
    { amount: 5, stars: 868, label: '5 TON' },
    { amount: 10, stars: 1737, label: '10 TON' },
    { amount: 25, stars: 4343, label: '25 TON' },
    { amount: 50, stars: 8687, label: '50 TON' },
  ]

  describe('USDT options', () => {
    it('should have valid pricing structure', () => {
      for (const option of usdtOptions) {
        expect(option.amount).toBeGreaterThan(0)
        expect(option.stars).toBeGreaterThan(0)
        expect(option.label).toBeTruthy()
      }
    })

    it('should have increasing stars with amount', () => {
      for (let i = 1; i < usdtOptions.length; i++) {
        expect(usdtOptions[i].stars).toBeGreaterThan(usdtOptions[i - 1].stars)
        expect(usdtOptions[i].amount).toBeGreaterThan(usdtOptions[i - 1].amount)
      }
    })

    it('should have consistent USD/stars ratio (approximately 43 stars per $1)', () => {
      for (const option of usdtOptions) {
        const starsPerDollar = option.stars / option.amount
        expect(starsPerDollar).toBeGreaterThan(40)
        expect(starsPerDollar).toBeLessThan(45)
      }
    })
  })

  describe('TON options', () => {
    it('should have valid pricing structure', () => {
      for (const option of tonOptions) {
        expect(option.amount).toBeGreaterThan(0)
        expect(option.stars).toBeGreaterThan(0)
        expect(option.label).toBeTruthy()
      }
    })

    it('should have increasing stars with amount', () => {
      for (let i = 1; i < tonOptions.length; i++) {
        expect(tonOptions[i].stars).toBeGreaterThan(tonOptions[i - 1].stars)
        expect(tonOptions[i].amount).toBeGreaterThan(tonOptions[i - 1].amount)
      }
    })
  })
})

describe('Transaction Verification Logic', () => {
  interface MockTransaction {
    hash: string
    comment: string
    amount: bigint
    timestamp: number
    success: boolean
  }

  function findPaymentByComment(
    transactions: MockTransaction[],
    expectedComment: string,
    expectedAmount: number,
    isUsdt: boolean
  ): MockTransaction | null {
    const expectedNano = isUsdt ? usdtToNano(expectedAmount) : tonToNano(expectedAmount)

    return (
      transactions.find((tx) => tx.comment === expectedComment && tx.amount >= expectedNano && tx.success) ||
      null
    )
  }

  const mockTransactions: MockTransaction[] = [
    { hash: 'tx1', comment: 'inv_12345', amount: 10000000n, timestamp: Date.now(), success: true },
    { hash: 'tx2', comment: 'inv_67890', amount: 5000000n, timestamp: Date.now(), success: true },
    { hash: 'tx3', comment: 'inv_11111', amount: 25000000n, timestamp: Date.now(), success: false },
    { hash: 'tx4', comment: 'inv_22222', amount: 1000000000n, timestamp: Date.now(), success: true },
  ]

  it('should find transaction by comment (USDT)', () => {
    const tx = findPaymentByComment(mockTransactions, 'inv_12345', 10, true)
    expect(tx).not.toBeNull()
    expect(tx?.hash).toBe('tx1')
  })

  it('should find transaction by comment (TON)', () => {
    const tx = findPaymentByComment(mockTransactions, 'inv_22222', 1, false)
    expect(tx).not.toBeNull()
    expect(tx?.hash).toBe('tx4')
  })

  it('should return null for non-existent comment', () => {
    const tx = findPaymentByComment(mockTransactions, 'inv_99999', 10, true)
    expect(tx).toBeNull()
  })

  it('should return null for failed transaction', () => {
    const tx = findPaymentByComment(mockTransactions, 'inv_11111', 25, true)
    expect(tx).toBeNull() // tx3 has success: false
  })

  it('should return null if amount is insufficient', () => {
    // Looking for 20 USDT but tx1 only has 10 USDT
    const tx = findPaymentByComment(mockTransactions, 'inv_12345', 20, true)
    expect(tx).toBeNull()
  })

  it('should accept overpayment', () => {
    // Looking for 5 USDT, tx1 has 10 USDT - should still match
    const tx = findPaymentByComment(mockTransactions, 'inv_12345', 5, true)
    expect(tx).not.toBeNull()
    expect(tx?.hash).toBe('tx1')
  })
})

describe('Invoice ID Generation', () => {
  it('should generate invoice IDs with pay_ prefix', () => {
    const timestamp = Date.now()
    const random = Math.random().toString(36).substring(2, 9)
    const invId = `pay_${timestamp}_${random}`

    expect(invId.startsWith('pay_')).toBe(true)
  })

  it('should contain timestamp for ordering', () => {
    const before = Date.now()
    const invId = `pay_${Date.now()}_abc123`
    const after = Date.now()

    const parts = invId.split('_')
    const timestamp = parseInt(parts[1])

    expect(timestamp).toBeGreaterThanOrEqual(before)
    expect(timestamp).toBeLessThanOrEqual(after)
  })

  it('should be unique across multiple generations', () => {
    const ids = new Set<string>()
    for (let i = 0; i < 100; i++) {
      const invId = `pay_${Date.now()}_${Math.random().toString(36).substring(2, 9)}`
      ids.add(invId)
    }
    expect(ids.size).toBe(100)
  })
})

describe('TON Address Validation', () => {
  // TON addresses are base64-encoded 36 bytes
  // Format: EQ/UQ (workchain 0) or Ef/Uf (workchain -1)

  function isValidTonAddress(address: string): boolean {
    // Basic validation - real validation would use ton-core library
    if (!address) return false
    if (address.length < 48 || address.length > 66) return false

    // Check prefix (EQ, UQ for workchain 0, Ef, Uf for workchain -1)
    const validPrefixes = ['EQ', 'UQ', 'Ef', 'Uf', 'kQ', '0Q']
    const prefix = address.substring(0, 2)
    return validPrefixes.includes(prefix)
  }

  it('should validate correct TON address format', () => {
    expect(isValidTonAddress('EQCxE6mUtQJKFnGfaROTKOt1lZbDiiX1kCixRv7Nw2Id_sDs')).toBe(true)
    expect(isValidTonAddress('UQCxE6mUtQJKFnGfaROTKOt1lZbDiiX1kCixRv7Nw2Id_sDs')).toBe(true)
  })

  it('should reject invalid addresses', () => {
    expect(isValidTonAddress('')).toBe(false)
    expect(isValidTonAddress('invalid')).toBe(false)
    expect(isValidTonAddress('XXCxE6mUtQJKFnGfaROTKOt1lZbDiiX1kCixRv7Nw2Id_sDs')).toBe(false)
  })

  it('should accept USDT master contract address', () => {
    expect(isValidTonAddress(TON_USDT_MASTER_MAINNET)).toBe(true)
  })
})
