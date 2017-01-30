TsSorter
========

This tool sorts TypeScript imports. Input and output is the clipboard.

Details
=======

It sorts import statements based on source file name.
```
import { B } from 'b';
import { C } from 'c';
import { A } from 'a';
```
->
```
import { A } from 'a';
import { B } from 'b';
import { C } from 'c';
```

It honors import groups (does not join them like some other tools do).
```
import { Z } from 'z';
import { Y } from '@y';

import { X } from 'x';
import { A } from 'a';
```
->
```
import { Y } from '@y';
import { Z } from 'z';

import { A } from 'a';
import { X } from 'x';
```

It sorts named imports.
```
import { Xz, Xy, Xx } from 'x';
import { Aca, Aaa, Aba } from './a';
```
->
```
import { Aaa, Aba, Aca } from './a';
import { Xx, Xy, Xz } from 'x';
```

It merges imports from same source file.
```
import {X} from 'x';
import { B } from 'a';
import { A } from 'a';
```
->
```
import { A, B } from 'a';
import { X } from 'x';
```

Installation
============

0. Install [stack](http://haskellstack.org) and requirements for [Hclip](https://github.com/jetho/Hclip) (on Linux tested with `xclip`).
1. Run `stack build`.
2. Locate compiled exe file (e.g. `.stack-work/install/x86_64-linux/ghc-8.0.2/8.0.2/bin/ts-sorter-exe`) and run it, or use command `stack exec ts-sorter-exe` to run executable for you.

Running tests
============
Parsing tests can be run via `stack test` from `/`.
System tests can be executed from directory `/res` via `./DoTests.hs`. Note that you have to first install dependencies (install [Node.js](https://nodejs.org/en/) and then run `npm install` in `/res` directory).

License
=======
GPLv3
