import { useSetAtom } from 'jotai';
import { useCallback } from 'react';
import { addToastAtom, type ToastType } from '@/atoms/toast';

interface ToastOptions {
  title?: string;
  duration?: number;
}

export function useToast() {
  const addToast = useSetAtom(addToastAtom);

  const toast = useCallback(
    (type: ToastType, message: string, options?: ToastOptions) => {
      addToast({
        type,
        message,
        title: options?.title,
        duration: options?.duration,
      });
    },
    [addToast]
  );

  const success = useCallback(
    (message: string, options?: ToastOptions) => toast('success', message, options),
    [toast]
  );

  const error = useCallback(
    (message: string, options?: ToastOptions) => toast('error', message, options),
    [toast]
  );

  const warning = useCallback(
    (message: string, options?: ToastOptions) => toast('warning', message, options),
    [toast]
  );

  const info = useCallback(
    (message: string, options?: ToastOptions) => toast('info', message, options),
    [toast]
  );

  return { toast, success, error, warning, info };
}
