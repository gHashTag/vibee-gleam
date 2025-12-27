import { useState, useCallback, useEffect } from 'react';
import { useNavigate } from 'react-router-dom';
import { Search as SearchIcon, TrendingUp, Users, X } from 'lucide-react';
import { useLanguage } from '@/hooks/useLanguage';
import { Skeleton } from '@/components/Skeleton';
import './Search.css';

interface SearchResult {
  id: string;
  type: 'user' | 'template';
  title: string;
  subtitle?: string;
  thumbnail?: string;
  username?: string;
}

export function SearchPage() {
  const { t } = useLanguage();
  const navigate = useNavigate();
  const [query, setQuery] = useState('');
  const [results, setResults] = useState<SearchResult[]>([]);
  const [loading, setLoading] = useState(false);
  const [trending] = useState<string[]>([
    'AI avatar',
    'lipsync',
    'talking head',
    'video template',
    'music video',
  ]);

  const handleSearch = useCallback(async (searchQuery: string) => {
    if (!searchQuery.trim()) {
      setResults([]);
      return;
    }

    setLoading(true);
    // TODO: Implement actual search API call
    // For now, simulate search delay
    await new Promise(resolve => setTimeout(resolve, 500));
    setResults([]);
    setLoading(false);
  }, []);

  useEffect(() => {
    const debounce = setTimeout(() => {
      handleSearch(query);
    }, 300);
    return () => clearTimeout(debounce);
  }, [query, handleSearch]);

  const handleResultClick = (result: SearchResult) => {
    if (result.type === 'user' && result.username) {
      navigate(`/${result.username}`);
    }
  };

  const handleTrendingClick = (tag: string) => {
    setQuery(tag);
  };

  const clearSearch = () => {
    setQuery('');
    setResults([]);
  };

  return (
    <div className="search-page">
      {/* Search Header */}
      <div className="search-header">
        <div className="search-input-wrapper">
          <SearchIcon size={20} className="search-input-icon" />
          <input
            type="text"
            className="search-input"
            placeholder={t('search.placeholder')}
            value={query}
            onChange={(e) => setQuery(e.target.value)}
            autoFocus
          />
          {query && (
            <button className="search-clear" onClick={clearSearch}>
              <X size={18} />
            </button>
          )}
        </div>
      </div>

      {/* Search Content */}
      <div className="search-content">
        {loading ? (
          <div className="search-loading">
            {[1, 2, 3, 4, 5].map((i) => (
              <div key={i} className="search-result-skeleton">
                <Skeleton width={48} height={48} borderRadius={24} />
                <div className="search-result-skeleton-text">
                  <Skeleton width="60%" height={16} />
                  <Skeleton width="40%" height={12} />
                </div>
              </div>
            ))}
          </div>
        ) : results.length > 0 ? (
          <div className="search-results">
            {results.map((result) => (
              <button
                key={result.id}
                className="search-result"
                onClick={() => handleResultClick(result)}
              >
                <div className="search-result-avatar">
                  {result.thumbnail ? (
                    <img src={result.thumbnail} alt={result.title} />
                  ) : (
                    <Users size={24} />
                  )}
                </div>
                <div className="search-result-info">
                  <span className="search-result-title">{result.title}</span>
                  {result.subtitle && (
                    <span className="search-result-subtitle">{result.subtitle}</span>
                  )}
                </div>
              </button>
            ))}
          </div>
        ) : query ? (
          <div className="search-empty">
            <SearchIcon size={48} />
            <span>{t('search.no_results')}</span>
          </div>
        ) : (
          <>
            {/* Trending Section */}
            <div className="search-section">
              <div className="search-section-header">
                <TrendingUp size={18} />
                <span>{t('search.trending')}</span>
              </div>
              <div className="search-tags">
                {trending.map((tag) => (
                  <button
                    key={tag}
                    className="search-tag"
                    onClick={() => handleTrendingClick(tag)}
                  >
                    #{tag}
                  </button>
                ))}
              </div>
            </div>

            {/* Discover Section */}
            <div className="search-section">
              <div className="search-section-header">
                <Users size={18} />
                <span>{t('search.discover')}</span>
              </div>
              <div className="search-discover-empty">
                <span>{t('search.discover_hint')}</span>
              </div>
            </div>
          </>
        )}
      </div>
    </div>
  );
}

export default SearchPage;
