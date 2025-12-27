import { atom } from 'jotai';

export type ToastType = 'success' | 'error' | 'warning' | 'info';

export interface ToastAction {
  label: string;
  onClick: () => void;
}

export interface Toast {
  id: string;
  type: ToastType;
  message: string;
  title?: string;
  duration?: number;
  action?: ToastAction;
}

export const toastsAtom = atom<Toast[]>([]);

let toastId = 0;

export const addToastAtom = atom(
  null,
  (get, set, toast: Omit<Toast, 'id'>) => {
    const id = `toast-${++toastId}`;
    const newToast: Toast = {
      ...toast,
      id,
      duration: toast.duration ?? 5000,
    };
    set(toastsAtom, [...get(toastsAtom), newToast]);
    return id;
  }
);

export const removeToastAtom = atom(null, (get, set, id: string) => {
  set(
    toastsAtom,
    get(toastsAtom).filter((t) => t.id !== id)
  );
});

// Helper hook for components
export const useToastActions = () => {
  return {
    addToastAtom,
    removeToastAtom,
  };
};
