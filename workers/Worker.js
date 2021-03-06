importScripts("https://dagrejs.github.io/project/dagre/latest/dagre.min.js");
onmessage = function(message) {
    console.log("Worker says", message)
    switch (message.data.action) {
        case "LayoutGraph":
            console.log("Asking for layout")
            // Create a new directed graph 
            var g = new dagre.graphlib.Graph();

            // Set an object for the graph label
            g.setGraph({});
            message.data.nodes.forEach(node => {
                g.setNode(node.id, node) 
            });
            message.data.edges.forEach(edge => {
                g.setEdge(edge.from, edge.to, edge)
            });
            dagre.layout(g);
            let placedNodes = []
            g.nodes().forEach(function(v) {
//                console.log("Node " + v + ": " + JSON.stringify(g.node(v)));
                let n = g.node(v)
                placedNodes.push({
                    id: n.id,
                    label: n.label,
                    width: n.width,
                    height: n.height,
                    x: n.x,
                    y: n.y
                });
            });
            console.log("placedNodes", placedNodes);
            let placedEdges = []
            g.edges().forEach(function(e) {
//                console.log("Edge " + e.v + " -> " + e.w + ": " + JSON.stringify(g.edge(e)));
                let edge = g.edge(e)
                placedEdges.push({
                    from: parseInt(e.v),  // seems to be a bug in dagre as it always return string here and below
                    to: parseInt(e.w),
                    label: edge.label,
                    width: edge.width,
                    height: edge.height,
                    x: edge.x,
                    y: edge.y,
                    points: edge.points
                });
            });
            console.log("placedEdges", placedEdges);
            console.log("Got a dagre graph", g._label);
            postMessage({
                graph:  {
                            width:g._label.width,
                            height:g._label.height
                        },
                nodes: placedNodes,
                edges: placedEdges
                })
            break;
        default:
            break;
    }
}