import { useState, useCallback } from 'react';
import { Heart } from 'lucide-react';
import './LikeAnimation.css';

interface LikeAnimationProps {
  onDoubleTap?: () => void;
  children: React.ReactNode;
}

export function LikeAnimation({ onDoubleTap, children }: LikeAnimationProps) {
  const [hearts, setHearts] = useState<{ id: number; x: number; y: number }[]>([]);
  const lastTapRef = { current: 0 };

  const handleClick = useCallback((e: React.MouseEvent | React.TouchEvent) => {
    const now = Date.now();
    const timeSinceLastTap = now - lastTapRef.current;
    lastTapRef.current = now;

    if (timeSinceLastTap < 300) {
      // Double tap detected
      const rect = (e.currentTarget as HTMLElement).getBoundingClientRect();
      let clientX: number, clientY: number;

      if ('touches' in e) {
        clientX = e.changedTouches[0].clientX;
        clientY = e.changedTouches[0].clientY;
      } else {
        clientX = e.clientX;
        clientY = e.clientY;
      }

      const x = clientX - rect.left;
      const y = clientY - rect.top;

      const heartId = Date.now();
      setHearts(prev => [...prev, { id: heartId, x, y }]);

      // Haptic feedback
      if ('vibrate' in navigator) {
        navigator.vibrate([15, 50, 15]);
      }

      onDoubleTap?.();

      // Remove heart after animation
      setTimeout(() => {
        setHearts(prev => prev.filter(h => h.id !== heartId));
      }, 1000);
    }
  }, [onDoubleTap]);

  return (
    <div
      className="like-animation-container"
      onClick={handleClick}
      onTouchEnd={handleClick}
    >
      {children}
      {hearts.map(heart => (
        <div
          key={heart.id}
          className="like-heart"
          style={{
            left: heart.x,
            top: heart.y,
          }}
        >
          <Heart size={80} fill="#ef4444" color="#ef4444" />
        </div>
      ))}
    </div>
  );
}
