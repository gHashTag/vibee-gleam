import { useCallback, useRef, useState } from 'react';
import { Header } from '@/components/Header';
import { FeedPanel } from '@/components/Panels/FeedPanel';
import { VerticalTabs } from '@/components/Navigation';
import { usePullToRefresh } from '@/hooks/usePullToRefresh';
import { useSetAtom } from 'jotai';
import { loadFeedAtom } from '@/atoms';
import { useIsTablet } from '@/hooks/useMediaQuery';
import './Feed.css';

export function FeedPage() {
  const containerRef = useRef<HTMLDivElement>(null);
  const loadFeed = useSetAtom(loadFeedAtom);
  const [isRefreshing, setIsRefreshing] = useState(false);
  const isTablet = useIsTablet();

  const handleRefresh = useCallback(async () => {
    setIsRefreshing(true);
    await loadFeed(true);
    setIsRefreshing(false);
  }, [loadFeed]);

  usePullToRefresh({
    containerRef,
    onRefresh: handleRefresh,
    isRefreshing,
  });

  return (
    <div className="feed-page">
      <Header />
      <main className="feed-main" ref={containerRef}>
        {/* Vertical tabs navigation (hidden on tablet) */}
        {!isTablet && <VerticalTabs />}

        <section className="feed-content">
          <div className={`pull-indicator ${isRefreshing ? 'refreshing' : ''}`}>
            <div className="pull-spinner" />
          </div>
          <FeedPanel fullscreen />
        </section>
      </main>
    </div>
  );
}

export default FeedPage;
