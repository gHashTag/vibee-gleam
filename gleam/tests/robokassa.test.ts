/**
 * Robokassa Signature Validation - Unit Tests
 *
 * Tests MD5 signature validation for Robokassa webhooks.
 * These tests run without the MCP server.
 *
 * Run: npm test -- robokassa.test.ts
 */

import { describe, it, expect } from 'vitest'
import { createHash } from 'crypto'

// Helper functions that mirror the Gleam/Erlang implementation
function md5Hash(data: string): string {
  return createHash('md5').update(data).digest('hex').toUpperCase()
}

function validateRobokassaSignature(
  outSum: string,
  invId: string,
  password2: string,
  signature: string
): boolean {
  const dataToHash = `${outSum}:${invId}:${password2}`
  const calculated = md5Hash(dataToHash)
  return calculated.toUpperCase() === signature.toUpperCase()
}

function generateRobokassaUrl(
  merchantLogin: string,
  outSum: string,
  invId: string,
  description: string,
  password1: string
): string {
  const signature = md5Hash(`${merchantLogin}:${outSum}:${invId}:${password1}`)
  const encodedDesc = encodeURIComponent(description)
  return `https://auth.robokassa.ru/Merchant/Index.aspx?MerchantLogin=${merchantLogin}&OutSum=${outSum}&InvId=${invId}&Description=${encodedDesc}&SignatureValue=${signature}`
}

describe('Robokassa Signature Validation', () => {
  describe('MD5 hash generation', () => {
    it('should generate correct MD5 hash', () => {
      // Known MD5 hash for "test"
      const hash = md5Hash('test')
      expect(hash).toBe('098F6BCD4621D373CADE4E832627B4F6')
    })

    it('should generate correct MD5 for Robokassa format', () => {
      const outSum = '100.00'
      const invId = '12345'
      const password = 'secret'
      const dataToHash = `${outSum}:${invId}:${password}`

      const hash = md5Hash(dataToHash)
      expect(hash).toBeTruthy()
      expect(hash.length).toBe(32) // MD5 produces 32 hex characters
    })
  })

  describe('Signature validation', () => {
    it('should validate correct signature', () => {
      const outSum = '100.00'
      const invId = '12345'
      const password2 = 'test_password'

      // Generate the expected signature
      const expectedSignature = md5Hash(`${outSum}:${invId}:${password2}`)

      const isValid = validateRobokassaSignature(outSum, invId, password2, expectedSignature)
      expect(isValid).toBe(true)
    })

    it('should reject incorrect signature', () => {
      const outSum = '100.00'
      const invId = '12345'
      const password2 = 'test_password'

      const isValid = validateRobokassaSignature(outSum, invId, password2, 'wrong_signature')
      expect(isValid).toBe(false)
    })

    it('should be case-insensitive for signature comparison', () => {
      const outSum = '100.00'
      const invId = '12345'
      const password2 = 'test_password'

      const signature = md5Hash(`${outSum}:${invId}:${password2}`)
      const lowerSignature = signature.toLowerCase()

      // Should accept both upper and lower case signatures
      expect(validateRobokassaSignature(outSum, invId, password2, signature)).toBe(true)
      expect(validateRobokassaSignature(outSum, invId, password2, lowerSignature)).toBe(true)
    })

    it('should fail with wrong password', () => {
      const outSum = '100.00'
      const invId = '12345'

      const signatureWithPassword1 = md5Hash(`${outSum}:${invId}:password1`)

      // Validation with different password should fail
      const isValid = validateRobokassaSignature(outSum, invId, 'password2', signatureWithPassword1)
      expect(isValid).toBe(false)
    })

    it('should fail with modified amount', () => {
      const originalOutSum = '100.00'
      const modifiedOutSum = '200.00'
      const invId = '12345'
      const password2 = 'test_password'

      // Signature generated with original amount
      const signature = md5Hash(`${originalOutSum}:${invId}:${password2}`)

      // Validation with modified amount should fail
      const isValid = validateRobokassaSignature(modifiedOutSum, invId, password2, signature)
      expect(isValid).toBe(false)
    })

    it('should fail with modified invoice ID', () => {
      const outSum = '100.00'
      const originalInvId = '12345'
      const modifiedInvId = '12346'
      const password2 = 'test_password'

      // Signature generated with original invoice ID
      const signature = md5Hash(`${outSum}:${originalInvId}:${password2}`)

      // Validation with modified invoice ID should fail
      const isValid = validateRobokassaSignature(outSum, modifiedInvId, password2, signature)
      expect(isValid).toBe(false)
    })
  })

  describe('Payment URL generation', () => {
    it('should generate valid Robokassa URL', () => {
      const url = generateRobokassaUrl('test_merchant', '100.00', '12345', 'Test payment', 'password1')

      expect(url).toContain('https://auth.robokassa.ru/Merchant/Index.aspx')
      expect(url).toContain('MerchantLogin=test_merchant')
      expect(url).toContain('OutSum=100.00')
      expect(url).toContain('InvId=12345')
      expect(url).toContain('Description=Test%20payment')
      expect(url).toContain('SignatureValue=')
    })

    it('should include correct signature in URL', () => {
      const merchantLogin = 'test_merchant'
      const outSum = '100.00'
      const invId = '12345'
      const password1 = 'password1'

      const expectedSignature = md5Hash(`${merchantLogin}:${outSum}:${invId}:${password1}`)
      const url = generateRobokassaUrl(merchantLogin, outSum, invId, 'Test', password1)

      expect(url).toContain(`SignatureValue=${expectedSignature}`)
    })

    it('should URL-encode description', () => {
      const url = generateRobokassaUrl(
        'test_merchant',
        '100.00',
        '12345',
        'Payment for VIBEE stars (100 stars)',
        'password1'
      )

      expect(url).toContain('Description=Payment%20for%20VIBEE%20stars%20(100%20stars)')
    })

    it('should handle cyrillic description', () => {
      const url = generateRobokassaUrl(
        'test_merchant',
        '500.00',
        '67890',
        'Оплата за звезды VIBEE',
        'password1'
      )

      // Should contain URL-encoded cyrillic text
      expect(url).toContain('Description=')
      expect(url).not.toContain('Оплата') // Should be encoded
    })
  })

  describe('Amount formatting', () => {
    it('should format whole numbers with decimals', () => {
      // Test various amount formats
      const testCases = [
        { input: 100, expected: '100.00' },
        { input: 500, expected: '500.00' },
        { input: 1000, expected: '1000.00' },
        { input: 99.5, expected: '99.50' },
        { input: 99.99, expected: '99.99' },
      ]

      for (const { input, expected } of testCases) {
        const formatted = input.toFixed(2)
        expect(formatted).toBe(expected)
      }
    })

    it('should round to 2 decimal places', () => {
      const amount = 100.999
      const formatted = amount.toFixed(2)
      expect(formatted).toBe('101.00')
    })
  })

  describe('Invoice ID format', () => {
    it('should generate unique invoice IDs', () => {
      const ids = new Set<string>()
      for (let i = 0; i < 1000; i++) {
        const id = `pay_${Date.now()}_${Math.random().toString(36).substring(2, 9)}`
        ids.add(id)
      }
      // All IDs should be unique
      expect(ids.size).toBe(1000)
    })

    it('should have expected format', () => {
      const timestamp = Date.now()
      const random = Math.random().toString(36).substring(2, 9)
      const invId = `pay_${timestamp}_${random}`

      expect(invId).toMatch(/^pay_\d+_[a-z0-9]+$/)
    })
  })
})

describe('Robokassa Pricing Options', () => {
  const rubOptions = [
    { amount: 100, stars: 43, label: '100 RUB' },
    { amount: 500, stars: 217, label: '500 RUB' },
    { amount: 1000, stars: 434, label: '1000 RUB' },
    { amount: 2000, stars: 869, label: '2000 RUB' },
    { amount: 5000, stars: 2173, label: '5000 RUB' },
  ]

  it('should have valid pricing structure', () => {
    for (const option of rubOptions) {
      expect(option.amount).toBeGreaterThan(0)
      expect(option.stars).toBeGreaterThan(0)
      expect(option.label).toBeTruthy()
    }
  })

  it('should have increasing stars with amount', () => {
    for (let i = 1; i < rubOptions.length; i++) {
      expect(rubOptions[i].stars).toBeGreaterThan(rubOptions[i - 1].stars)
      expect(rubOptions[i].amount).toBeGreaterThan(rubOptions[i - 1].amount)
    }
  })

  it('should have consistent stars/RUB ratio (approximately 2.3 RUB per star)', () => {
    for (const option of rubOptions) {
      const rubPerStar = option.amount / option.stars
      // Rate should be approximately 2.3 RUB per star (with some variance)
      expect(rubPerStar).toBeGreaterThan(2.0)
      expect(rubPerStar).toBeLessThan(2.5)
    }
  })
})
