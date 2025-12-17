import {
  Header,
  Hero,
  Features,
  Integrations,
  HowItWorks,
  Pricing,
  Footer,
} from '@/components/landing';
import './Home.css';

export function HomePage() {
  return (
    <div className="landing-page">
      <Header />
      <main>
        <Hero />
        <Features />
        <Integrations />
        <HowItWorks />
        <Pricing />
      </main>
      <Footer />
    </div>
  );
}
