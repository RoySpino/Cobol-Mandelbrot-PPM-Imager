# Mandelbrot Set Generator (COBOL)

A retro-style implementation of the **Mandelbrot Set** fractal generator written in COBOL. This program calculates the complex mathematical set and outputs the result as a high-quality **PPM (Portable PixMap)** image file.
This program was origianlly run on a PC running a AMD Athlon II X2 250 and took **7.5 hours** to generate a 5k x 5k image.
As of 2026-09-24 the current time to complete on the same machine is: **58 Minutes**


## Overview
While COBOL is traditionally associated with business logic and banking, this project demonstrates the language's capability in performing complex mathematical iterations and generating visual data. 

The program maps a grid of pixels to the complex plane, iterates the fractal formula $z_{n+1} = z_n^2 + c$, and applies a logarithmic "smooth coloring" algorithm to create a smooth gradient of colors rather than flat bands.

## Features
- **Fractal Calculation**: Accurate iteration of the Mandelbrot set.
- **Smooth Coloring**: Uses logarithmic math to produce a smooth color transition.
- **PPM Output**: Generates a `.ppm` file, which can be opened by most standard image viewers (e.g., GIMP, IrfanView, or converted to PNG/JPG).
- **Classic Architecture**: Traditional COBOL structure including `IDENTIFICATION`, `ENVIRONMENT`, `DATA`, and `PROCEDURE` divisions.
- **Navigation**: the variables `CX`, `CY` and `ZOOM` allow you to navigate and zoom through the fractal.


## Installation & Requirements

To run this program, you will need a COBOL compiler. The recommended compiler is **GnuCOBOL** (formerly `cobc`).

### Prerequisites
- **GnuCOBOL**: [Install via your package manager]
  - Ubuntu/Debian: `sudo apt-get install gcc-cobc`
  - macOS (Homebrew): `brew install gcc-cobc`
  - Windows: [Download the GnuCOBOL installer]

## Getting Started

1. **Clone the repository:**
   ```bash
   git clone https://github.com/RoySpino/Cobol-Mandelbrot-PPM-Imager.git
   cd Cobol-Mandelbrot-PPM-Imager
   ```

2. **Compile the program:**
   Use the following command to compile the `.cob` file into an executable:
   ```bash
   cobc -xj -o mset *.cob
   ```
   *(Note: The `-xj` flag tells the compiler to produce an executable file.)*

3. **Run the program:**
   ```bash
   ./mset
   ```

4. **View the result:**
   After running, the program will generate a file named `mout.ppm`. Open it with your favorite image viewer.

## How It Works
1. **Coordinate Mapping**: The program translates the image dimensions (e.g., 5000x5000) into a coordinate system on the complex plane.
2. **The Loop**: For every pixel, the program performs the "Mandelbrot" iteration. If the value escapes a certain threshold, it is assigned a "color" based on how quickly it escaped.
3. **Color Scaling**: The `GET-PIXEL` logic uses a mathematical formula to map the escape time to an RGB color space, ensuring a smooth aesthetic.
4. **Output**: The `DRAW` and `SET-HEADER` routines write the raw RGB values into the PPM format.

## File Structure
- `mSet.cob`: The main COBOL source code.
- `mout.ppm`: The generated fractal image (created after execution).
- `Makefile`: A Makefile for compiling the COBOL program

## License
This project is open-source and available under the [MIT License](LICENSE).
