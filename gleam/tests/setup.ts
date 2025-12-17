/**
 * Vitest Global Setup for VIBEE Payment Tests
 */

import { vi, beforeEach, afterEach } from 'vitest'

// Environment Variables
process.env.NODE_ENV = 'test'
process.env.ROBOKASSA_MERCHANT_LOGIN = 'test_merchant'
process.env.ROBOKASSA_PASSWORD1 = 'test_password1'
process.env.ROBOKASSA_PASSWORD2 = 'test_password2'
process.env.ROBOKASSA_TEST_MODE = 'true'
process.env.TON_WALLET_ADDRESS = 'UQTestWalletAddress'
process.env.TON_API_KEY = 'test_ton_api_key'
process.env.TON_USDT_MASTER = 'EQCxE6mUtQJKFnGfaROTKOt1lZbDiiX1kCixRv7Nw2Id_sDs'
process.env.TON_NETWORK = 'testnet'

// Global Hooks
beforeEach(() => {
  vi.clearAllMocks()
})

afterEach(() => {
  vi.useRealTimers()
})

// MCP WebSocket Helper
export interface McpToolResult {
  success: boolean
  data?: unknown
  error?: string
}

export async function callMcpTool(
  ws: WebSocket,
  toolName: string,
  args: Record<string, unknown>
): Promise<McpToolResult> {
  return new Promise((resolve, reject) => {
    const timeout = setTimeout(() => {
      reject(new Error('MCP call timeout'))
    }, 10000)

    const messageId = Math.random().toString(36).substring(7)

    const handler = (event: MessageEvent) => {
      try {
        const data = JSON.parse(event.data)
        if (data.id === messageId) {
          clearTimeout(timeout)
          ws.removeEventListener('message', handler)
          if (data.error) {
            resolve({ success: false, error: data.error.message })
          } else {
            resolve({ success: true, data: data.result })
          }
        }
      } catch {
        // Ignore parse errors
      }
    }

    ws.addEventListener('message', handler)

    ws.send(
      JSON.stringify({
        jsonrpc: '2.0',
        id: messageId,
        method: 'tools/call',
        params: {
          name: toolName,
          arguments: args,
        },
      })
    )
  })
}

// MD5 Hash Helper (for Robokassa signature testing)
import { createHash } from 'crypto'

export function md5(data: string): string {
  return createHash('md5').update(data).digest('hex')
}

export function createRobokassaSignature(
  outSum: string,
  invId: string,
  password: string
): string {
  return md5(`${outSum}:${invId}:${password}`).toUpperCase()
}
