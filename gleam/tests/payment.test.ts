/**
 * VIBEE Payment MCP Tools - Integration Tests
 *
 * Tests the payment tools via WebSocket MCP protocol.
 * Run: npm test -- payment.test.ts
 */

import { describe, it, expect, beforeAll, afterAll, vi } from 'vitest'
import WebSocket from 'ws'
import { callMcpTool, createRobokassaSignature } from './setup'

const MCP_URL = process.env.MCP_URL || 'ws://localhost:8080/ws/mcp'

describe('Payment MCP Tools', () => {
  let ws: WebSocket

  beforeAll(async () => {
    // Skip if MCP server is not running
    try {
      ws = new WebSocket(MCP_URL)
      await new Promise<void>((resolve, reject) => {
        ws.on('open', () => resolve())
        ws.on('error', () => reject(new Error('MCP server not available')))
        setTimeout(() => reject(new Error('Connection timeout')), 5000)
      })
    } catch {
      console.warn('MCP server not available, skipping integration tests')
      ws = null as unknown as WebSocket
    }
  })

  afterAll(() => {
    if (ws && ws.readyState === WebSocket.OPEN) {
      ws.close()
    }
  })

  describe('payment_create', () => {
    it.skipIf(!ws)('should create Robokassa payment', async () => {
      const result = await callMcpTool(ws, 'payment_create', {
        method: 'robokassa',
        amount: 100,
        telegram_id: 123456789,
      })

      expect(result.success).toBe(true)
      const data = result.data as Record<string, unknown>
      expect(data.inv_id).toBeDefined()
      expect(typeof data.inv_id).toBe('string')
      expect(data.status).toBe('PENDING')
      expect(data.invoice_url).toContain('robokassa')
    })

    it.skipIf(!ws)('should create TON USDT payment', async () => {
      const result = await callMcpTool(ws, 'payment_create', {
        method: 'ton_usdt',
        amount: 10,
        telegram_id: 123456789,
      })

      expect(result.success).toBe(true)
      const data = result.data as Record<string, unknown>
      expect(data.inv_id).toBeDefined()
      expect(data.status).toBe('PENDING')
      expect(data.invoice_url).toContain('tonkeeper')
    })

    it.skipIf(!ws)('should create TON native payment', async () => {
      const result = await callMcpTool(ws, 'payment_create', {
        method: 'ton_native',
        amount: 5,
        telegram_id: 123456789,
      })

      expect(result.success).toBe(true)
      const data = result.data as Record<string, unknown>
      expect(data.inv_id).toBeDefined()
      expect(data.status).toBe('PENDING')
    })

    it.skipIf(!ws)('should reject invalid payment method', async () => {
      const result = await callMcpTool(ws, 'payment_create', {
        method: 'invalid_method',
        amount: 100,
        telegram_id: 123456789,
      })

      expect(result.success).toBe(false)
      expect(result.error).toContain('method')
    })

    it.skipIf(!ws)('should reject missing telegram_id', async () => {
      const result = await callMcpTool(ws, 'payment_create', {
        method: 'robokassa',
        amount: 100,
      })

      expect(result.success).toBe(false)
      expect(result.error).toContain('telegram_id')
    })
  })

  describe('payment_status', () => {
    it.skipIf(!ws)('should return status for existing payment', async () => {
      // First create a payment
      const createResult = await callMcpTool(ws, 'payment_create', {
        method: 'robokassa',
        amount: 100,
        telegram_id: 123456789,
      })

      expect(createResult.success).toBe(true)
      const invId = (createResult.data as Record<string, unknown>).inv_id as string

      // Then check status
      const statusResult = await callMcpTool(ws, 'payment_status', {
        inv_id: invId,
      })

      expect(statusResult.success).toBe(true)
      const data = statusResult.data as Record<string, unknown>
      expect(data.status).toBe('PENDING')
      expect(data.inv_id).toBe(invId)
    })

    it.skipIf(!ws)('should return error for non-existent payment', async () => {
      const result = await callMcpTool(ws, 'payment_status', {
        inv_id: 'non_existent_payment_12345',
      })

      expect(result.success).toBe(false)
      expect(result.error).toContain('not found')
    })
  })

  describe('payment_verify', () => {
    it.skipIf(!ws)('should verify valid Robokassa signature', async () => {
      // Create a payment first
      const createResult = await callMcpTool(ws, 'payment_create', {
        method: 'robokassa',
        amount: 100,
        telegram_id: 123456789,
      })

      const invId = (createResult.data as Record<string, unknown>).inv_id as string
      const outSum = '100.00'

      // Generate valid signature using test password2
      const signature = createRobokassaSignature(outSum, invId, 'test_password2')

      const result = await callMcpTool(ws, 'payment_verify', {
        method: 'robokassa',
        inv_id: invId,
        out_sum: outSum,
        signature: signature,
      })

      expect(result.success).toBe(true)
      const data = result.data as Record<string, unknown>
      expect(data.valid).toBe(true)
    })

    it.skipIf(!ws)('should reject invalid Robokassa signature', async () => {
      const result = await callMcpTool(ws, 'payment_verify', {
        method: 'robokassa',
        inv_id: 'test_123',
        out_sum: '100.00',
        signature: 'invalid_signature',
      })

      expect(result.success).toBe(true)
      const data = result.data as Record<string, unknown>
      expect(data.valid).toBe(false)
    })
  })

  describe('balance_get', () => {
    it.skipIf(!ws)('should return user balance', async () => {
      const result = await callMcpTool(ws, 'balance_get', {
        telegram_id: 123456789,
      })

      expect(result.success).toBe(true)
      const data = result.data as Record<string, unknown>
      expect(typeof data.stars_balance).toBe('number')
      expect(data.stars_balance).toBeGreaterThanOrEqual(0)
    })

    it.skipIf(!ws)('should return 0 for new user', async () => {
      const result = await callMcpTool(ws, 'balance_get', {
        telegram_id: 999999999,
      })

      expect(result.success).toBe(true)
      const data = result.data as Record<string, unknown>
      expect(data.stars_balance).toBe(0)
    })
  })

  describe('balance_topup_options', () => {
    it.skipIf(!ws)('should return Robokassa options', async () => {
      const result = await callMcpTool(ws, 'balance_topup_options', {
        method: 'robokassa',
      })

      expect(result.success).toBe(true)
      const data = result.data as { options: Array<{ amount: number; stars: number; label: string }> }
      expect(Array.isArray(data.options)).toBe(true)
      expect(data.options.length).toBeGreaterThan(0)

      // Check first option structure
      const firstOption = data.options[0]
      expect(firstOption).toHaveProperty('amount')
      expect(firstOption).toHaveProperty('stars')
      expect(firstOption).toHaveProperty('label')
    })

    it.skipIf(!ws)('should return TON USDT options', async () => {
      const result = await callMcpTool(ws, 'balance_topup_options', {
        method: 'ton_usdt',
      })

      expect(result.success).toBe(true)
      const data = result.data as { options: Array<{ amount: number; stars: number; label: string }> }
      expect(Array.isArray(data.options)).toBe(true)
      expect(data.options.length).toBeGreaterThan(0)
    })

    it.skipIf(!ws)('should return TON native options', async () => {
      const result = await callMcpTool(ws, 'balance_topup_options', {
        method: 'ton_native',
      })

      expect(result.success).toBe(true)
      const data = result.data as { options: Array<{ amount: number; stars: number; label: string }> }
      expect(Array.isArray(data.options)).toBe(true)
    })

    it.skipIf(!ws)('should return Telegram Stars options', async () => {
      const result = await callMcpTool(ws, 'balance_topup_options', {
        method: 'telegram',
      })

      expect(result.success).toBe(true)
      const data = result.data as { options: Array<{ amount: number; stars: number; label: string }> }
      expect(Array.isArray(data.options)).toBe(true)
    })
  })
})
