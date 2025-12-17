# VIBEE Remotion Project

This project uses [Remotion](https://www.remotion.dev) to programmatically create and render videos. It's a collection of video compositions that can be used to generate various types of video content, from simple text overlays to complex, data-driven videos with lip-syncing avatars.

## Project Structure

- `src/`: Contains the source code for the Remotion project.
- `src/compositions/`: This directory contains the individual video compositions. Each composition is a React component that defines a specific video layout or effect.
- `src/factory/`: This directory contains a template factory for generating variations of a "talking head" video.
- `player/`: A separate web-based player for the videos.
- `public/`: Contains static assets like images, videos, and music that are used in the compositions.
- `scripts/`: Contains scripts for automating tasks like rendering batches of videos.
- `whisper.cpp/`: Contains the Whisper speech-to-text model, which is likely used for generating subtitles.

## Available Compositions

This project includes a variety of compositions that can be rendered as videos. Here's a list of the available compositions and their purpose:

- **TextOverlay**: A simple composition that displays a title and subtitle with a background color.
- **VideoIntro**: A branded intro animation with a logo, brand name, and tagline.
- **DynamicVideo**: A data-driven composition that can display a user's name, a message, and an avatar.
- **LipSyncMain**: An Instagram Reels-style template with a lip-syncing avatar, background videos, and music.
- **LipSyncBusiness**: A business-themed version of the `LipSyncMain` composition.
- **SplitScreen**: A composition that displays 2-4 media items in a grid layout.
- **Carousel**: A slideshow of images and videos with transitions.
- **KenBurns**: A composition that applies cinematic pan and zoom effects to photos.
- **Subtitles**: A composition for displaying animated subtitles with various styles.
- **FactoryTalkingHead**: A template factory for generating variations of a "talking head" video.
- **SplitTalkingHead**: A composition that creates a split-screen effect with a talking head and b-roll footage.

## Building and Running

### Prerequisites

- Node.js and npm
- FFmpeg

### Installation

1.  Install the dependencies:

    ```bash
    npm install
    ```

### Running the Remotion Studio

The Remotion Studio is a web-based interface for previewing and editing your compositions. To start the studio, run the following command:

```bash
npm start
```

This will open the Remotion Studio in your web browser.

### Rendering a Video

To render a video, you can use the `remotion render` command. You need to specify the composition ID and the output file name. For example, to render the `TextOverlay` composition, you would run the following command:

```bash
remotion render TextOverlay out/text-overlay.mp4
```

You can also pass props to the composition using the `--props` flag. For example:

```bash
remotion render TextOverlay out/text-overlay.mp4 --props '{"title": "Hello, World!", "subtitle": "This is my first Remotion video!"}'
```

### Batch Rendering

The `scripts/batch-render.ts` script is a powerful tool for generating and rendering a large number of video variations. It uses the template factory in `src/factory` to generate variations of a template, and then it uses the Remotion renderer to render each variation.

The script takes several command-line arguments that allow you to control the rendering process, such as the template to use, the number of variations to generate, and the output directory. It also has a dry-run mode that allows you to see the variations that would be generated without actually rendering them.

Here's an example of how to use the script:

```bash
npx ts-node scripts/batch-render.ts --template talking-head --limit 10 --dry-run
```

This command will generate 10 variations of the `talking-head` template and output a manifest file without rendering any videos.

Here are some of the available command-line arguments:

- `--template`: The ID of the template to use (default: `talking-head`).
- `--limit`: The maximum number of variants to generate (default: 10).
- `--min-priority`: The minimum priority score for the variants to generate (default: 0).
- `--output`: The output directory (default: `./out`).
- `--dry-run`: If set, the script will only generate the variants and not render them.
- `--concurrency`: The number of parallel renders (default: 3).
- `--duration`: The duration of the video in seconds (default: 30).
- `--test-mode`: If set, the script will use placeholder assets.

## Template Factory

The `src/factory` directory contains a template factory for generating variations of a "talking head" video. The factory is used by the `scripts/batch-render.ts` script to generate a large number of video variations.

The template is defined in a JSON file, `src/factory/templates/talking-head.json`. This file defines the "axes" of variation, which are the different properties that can be changed to create different variations of the video. For each axis, it defines the possible values and their weights. The weights are used to control the probability of each value being selected when generating a variation.

The file also defines a set of `excludeCombinations`, which are combinations of axis values that should not be used together. This is useful for preventing the generation of variations that are not aesthetically pleasing or that do not make sense.

You can create new templates by creating new JSON files in the `src/factory/templates` directory and updating the `src/factory/index.ts` file to include the new template.


## Development Conventions

- **Compositions**: Each composition should be in its own file in the `src/compositions` directory.
- **Props**: Each composition should define a Zod schema for its props. This helps to ensure that the props are valid and provides type safety.
- **Styling**: The project uses CSS-in-JS for styling.
- **Assets**: Static assets should be placed in the `public` directory.

## Player Application

The `player` directory contains a separate React application that serves as a web-based player and editor for the Remotion videos. It allows you to preview the compositions, edit their properties, and see the changes in real-time.

### Running the Player

1.  Navigate to the `player` directory:

    ```bash
    cd player
    ```

2.  Install the dependencies:

    ```bash
    npm install
    ```

3.  Start the development server:

    ```bash
    npm run dev
    ```

This will open the player application in your web browser.

## Render Server

The `render-server.ts` file contains a server that can be used to render Remotion videos and stills. It also has a WebSocket server for real-time communication.

The server has the following features:

- It can render videos and stills asynchronously.
- It can track the progress of render jobs.
- It can serve rendered files.
- It can upload files to S3.
- It can list files in an S3 bucket.
- It has a WebSocket server for real-time communication.

### Running the Server

To run the server, you can use the following command:

```bash
npx ts-node render-server.ts
```

This will start the server on port 3333.
