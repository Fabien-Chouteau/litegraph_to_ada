# litegraph_to_ada

`litegraph_to_ada` provides a bridge between the LiteGraph browser graph editor
and Ada programs.

![](docs/LG_example_screenshot.png)

 - 1. Define node and link types in Ada ([example](tests/src/nodes-split.ads))
 - 2. Automaticllay export a LiteGraph definition of the Ada node/link types ([example](docs/example_nodes.js))
 - 3. Deploy a graph editor website on GitHub pages ([example](https://fabien-chouteau.github.io/litegraph_to_ada/))
 - 4. Use the website to design graphs and export their configuration
 - 5. Load the configurations in Ada
