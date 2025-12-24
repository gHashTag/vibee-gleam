// PaywallModal - Subscription paywall
// Shows when free renders are exhausted

import { useAtomValue, useSetAtom } from 'jotai';
import { showPaywallAtom, renderQuotaAtom, userAtom } from '@/atoms';
import { X, Zap, Star, Crown, CreditCard, Sparkles } from 'lucide-react';
import './styles.css';

const PLANS = [
  {
    id: 'junior',
    name: 'JUNIOR',
    price: 99,
    generations: 100,
    features: ['100 renders/month', 'HD quality', 'Email support'],
    popular: false,
  },
  {
    id: 'middle',
    name: 'MIDDLE',
    price: 299,
    generations: 500,
    features: ['500 renders/month', '4K quality', 'Priority support', 'Custom fonts'],
    popular: true,
  },
  {
    id: 'senior',
    name: 'SENIOR',
    price: 999,
    generations: null, // Unlimited
    features: ['Unlimited renders', '4K quality', 'Premium support', 'Custom fonts', 'API access'],
    popular: false,
  },
];

const PAYMENT_METHODS = [
  { id: 'robokassa', name: 'Robokassa', icon: CreditCard, label: 'Card' },
  { id: 'stars', name: 'Telegram Stars', icon: Star, label: 'Stars' },
  { id: 'ton', name: 'TON', icon: Sparkles, label: 'TON' },
];

interface PaywallModalProps {
  onClose?: () => void;
}

export function PaywallModal({ onClose }: PaywallModalProps) {
  const showPaywall = useAtomValue(showPaywallAtom);
  const setShowPaywall = useSetAtom(showPaywallAtom);
  const quota = useAtomValue(renderQuotaAtom);
  const user = useAtomValue(userAtom);

  const handleClose = () => {
    setShowPaywall(false);
    onClose?.();
  };

  const handleSubscribe = async (planId: string, paymentMethod: string) => {
    if (!user) return;

    // TODO: Integrate with payment API
    // For now, open Telegram bot for payment
    const plan = PLANS.find(p => p.id === planId);
    if (!plan) return;

    // Redirect to Telegram bot with payment deeplink
    const telegramUrl = `https://t.me/agent_vibecoder_bot?start=subscribe_${planId}_${paymentMethod}`;
    window.open(telegramUrl, '_blank');

    console.log('[Paywall] Subscribe clicked:', { planId, paymentMethod, user: user.id });
  };

  if (!showPaywall) return null;

  return (
    <div className="paywall-overlay" onClick={handleClose}>
      <div className="paywall-modal" onClick={(e) => e.stopPropagation()}>
        <button className="paywall-close" onClick={handleClose}>
          <X size={24} />
        </button>

        <div className="paywall-header">
          <Zap size={48} className="paywall-icon" />
          <h2>Free Renders Used Up!</h2>
          <p className="paywall-subtitle">
            You've used all {quota?.total_renders || 3} free renders.
            Subscribe to continue creating amazing videos.
          </p>
        </div>

        <div className="paywall-plans">
          {PLANS.map((plan) => (
            <div
              key={plan.id}
              className={`paywall-plan ${plan.popular ? 'popular' : ''}`}
            >
              {plan.popular && (
                <div className="plan-badge">Most Popular</div>
              )}

              <div className="plan-header">
                <h3 className="plan-name">{plan.name}</h3>
                <div className="plan-price">
                  <span className="price-amount">${plan.price}</span>
                  <span className="price-period">/month</span>
                </div>
              </div>

              <div className="plan-generations">
                {plan.generations === null ? (
                  <>
                    <Crown size={20} />
                    <span>Unlimited renders</span>
                  </>
                ) : (
                  <>
                    <Zap size={20} />
                    <span>{plan.generations} renders/month</span>
                  </>
                )}
              </div>

              <ul className="plan-features">
                {plan.features.map((feature, i) => (
                  <li key={i}>{feature}</li>
                ))}
              </ul>

              <div className="plan-payment-methods">
                {PAYMENT_METHODS.map((method) => (
                  <button
                    key={method.id}
                    className="payment-btn"
                    onClick={() => handleSubscribe(plan.id, method.id)}
                    title={`Pay with ${method.name}`}
                  >
                    <method.icon size={16} />
                    <span>{method.label}</span>
                  </button>
                ))}
              </div>
            </div>
          ))}
        </div>

        <div className="paywall-footer">
          <p>All payments are secure and processed via Telegram</p>
        </div>
      </div>
    </div>
  );
}
