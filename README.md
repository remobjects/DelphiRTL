<img src="https://docs.elementscompiler.com/API/DelphiRTL/DelphiRTL-256.png" width="128" align="right" />

# DelphiRTL

Part of the RTL2 project, this is a Delphi-compatible RTL and non-visual VCL library implementation for the Elements compiler (and more specifically, the Oxygene language).

## Goal

The goal of this project is to reproduce a reasonable subset of standard Delphi APIs used by a large percentage of Delphi code bases, in order to facilitate and ease the _porting_ of such code bases to Oxygene to re-use business logic in .NET, Cocoa, Java/Android or Island applications.

The goal is most decidedly *not* to get existing Delphi projects or applications to just recompile out of the box. The differences between the platforms are too significant, and there is only so much abstraction a library such as this can provide; some changes to exiting code will continue to be required for it to build in Oxygene, and some paradigms need to be rethought to fit in well with the platforms.

## Scope

The scope of this project is purposely limited to simple APIs, commonly used low-level helper functions (such as `SysUtils.pas`) and simple classes (such as `TStringList`), as well as to make core types such as `Object` and `String` more compatible with Delphi code. Decidedly *out of scope* are UI level libraries such as the VCL or FMX, Delphi's TDataSet-based database layer, or anything on that level.

## Implementation

This library is implemented fully from scratch, without referencing or reusing any of Delphi's or FPCs implementation of the same functionality. Merely the outward-facing API is kept identical and compatible with Delphi's version of the RTL/VCL.

Where possible, the library is targeted to work well for code coming from both older (ca. Delphi 6/7) and more recent Delphi version; while it does/will support AnsiStrings it does strongly favor UTF-16 unicode strings (aka `WideStrings`), as used both by newer Delphi versions _and_ all of Elements' target platforms.

This library is a work in progress; all functionality presented is useable now, and it will be expanded and grown over time.

Community contributions, pull requests and other feedback are highly welcome.

## Requirements

As of this writing, the Delphi RTL library requires the latest Elements 8.4 compiler to be built and to be used.

## See Also

* [RemObjects Elements Homepage](http://www.elementscompiler.com/)
* [RemObjects Elements Docs](http://docs.elementscompiler.com/)
* [Delphi RTL](http://docs.elementscompiler.com/API/DelphiRTL/) on docs.elementscompiler.com

