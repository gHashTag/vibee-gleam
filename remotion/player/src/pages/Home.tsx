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

function HomePage() {
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

export default HomePage;
