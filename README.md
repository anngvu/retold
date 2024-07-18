# retold (retelling of linked data)

Experimental Clojure library and CLI util for translation between different formats expected by different linked data tools/frameworks.
A "retelling" often implies that some details can be lost because no exact translation is possible. 

The different frameworks/formats under consideration:

- [Linked Data Modeling Language (LinkML)](https://github.com/linkml/linkml)
- [schematic](https://github.com/Sage-Bionetworks/schematic/) (schema.org json-ld)
- [Reasonable Ontology Templates (OTTR)](https://ottr.xyz/)


## Usage

This is expected to be run as a command-line tool with [babashka](https://babashka.org/).

### Paths

- [x] (limited) LinkML -> schematic: `bb ./retold as-jsonld --dir modules --out model.jsonld` 
- [ ] schematic -> LinkML
- [ ] LinkML -> OTTR 

## License

Copyright © 2023 FIXME

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
