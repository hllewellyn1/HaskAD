# HaskAD
HaskAD (Haskell-Aided Design) is a piece of CAD software written entirely in the Haskell programming language.

![Alt Text](https://github.com/hllewellyn1/HaskAD/blob/ad22c4781a0c582866a310d55123434b7ad04cce/HaskAD.gif)

## Features

- 3-axis rotation (permitting any viewpoint)
- Automatic wireframe generation
- Load/Save capability

## User Guide

The model can be rotated around the x, y and z axes by pressing the 'x', 'y' and 'z' keys respectively.

Pressing the spacebar brings up the point creation menu, within which there are three fields for cooridnate entry.  Each field can be selected by pressing the associated key ('x','y' or 'z').  Once the coordinate is as desired, press enter to confirm.  Pressing the spacebar again exits the menu.  

Note that successive points are automatically connected by wireframe.  To avoid this, close and reopen the menu between points you don't intend to connect.  You may have to specify a point more than once for more complicated wireframe models.

Pressing 's' saves the model to the savefile (overwriting any existing data).  When HaskAD is launched, it automatically loads from this savefile. You will notice that on first launch, HaskAD loads a 100x100 cube as a demo. 

Pressing the 'delete' key clears the model, allowing for a fresh start.

## Technical Details

- Built entirely in Haskell, making use of the gloss graphics library
- Only 338 lines of code

## Details for Building

HaskAD was built on a Windows machine using the Glasgow Haskell Compiler, and has not been tested on any other operating systems.

The gloss graphics library requires 'glut32.dll' to exist in the same directory as the executable, for it to run.  A quick internet search will help you source this.

##

[![Generic badge](https://img.shields.io/badge/VERSION-1.0-<COLOR>.svg)](https://github.com/hllewellyn1/HaskAD)
![Haskell](https://img.shields.io/badge/Haskell-5e5086?style=for-the-badge&logo=haskell&logoColor=white)
