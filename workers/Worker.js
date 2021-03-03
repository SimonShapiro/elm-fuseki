importScripts("https://dagrejs.github.io/project/dagre/latest/dagre.min.js")
onmessage = function(message) {
    console.log("Worker says", message)
    switch (message.data.action) {
        case "LayoutGraph":
            console.log("Asking for layout")
            // Create a new directed graph 
            var g = new dagre.graphlib.Graph();

            // Set an object for the graph label
            g.setGraph({});
            console.log("Got a dagre graph", g)
            break;
        default:
            break;
    }
}