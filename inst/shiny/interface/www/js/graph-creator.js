//document.onload = (function(d3, saveAs, Blob, undefined){
 // "use strict";

  // TODO add user settings
  var consts = {
    defaultTitle: "random variable"
  };
  var settings = {
    appendElSpec: "#graph"
  };
  // define graphcreator object
  var GraphCreator = function(svg, nodes, edges){
    var thisGraph = this;
        thisGraph.idct = 0;

    thisGraph.nodes = nodes || [];
    thisGraph.edges = edges || [];

    thisGraph.state = {
      selectedNode: null,
      selectedEdge: null,
      mouseDownNode: null,
      mouseDownLink: null,
      justDragged: false,
      justScaleTransGraph: false,
      lastKeyDown: -1,
      shiftNodeDrag: false,
      selectedText: null
    };

    // define arrow markers for graph links
    var defs = svg.append('svg:defs');
    defs.append('svg:marker')
      .attr('id', 'end-arrow')
      .attr('viewBox', '0 -5 10 10')
      .attr('refX', "32")
      .attr('markerWidth', 3.5)
      .attr('markerHeight', 3.5)
      .attr('orient', 'auto')
      .append('svg:path')
      .attr('d', 'M0,-5L10,0L0,5');

    // define arrow markers for leading arrow
    defs.append('svg:marker')
      .attr('id', 'mark-end-arrow')
      .attr('viewBox', '0 -5 10 10')
      .attr('refX', 7)
      .attr('markerWidth', 3.5)
      .attr('markerHeight', 3.5)
      .attr('orient', 'auto')
      .append('svg:path')
      .attr('d', 'M0,-5L10,0L0,5');

    thisGraph.svg = svg;
    thisGraph.svgG = svg.append("g")
          .classed(thisGraph.consts.graphClass, true);
    var svgG = thisGraph.svgG;

    // displayed when dragging between nodes
    thisGraph.dragLine = svgG.append('svg:path')
          .attr('class', 'link dragline hidden')
          .attr('d', 'M0,0L0,0')
          .style('marker-end', 'url(#mark-end-arrow)');

    // svg nodes and edges
    thisGraph.paths = svgG.append("g").selectAll("g");
    thisGraph.circles = svgG.append("g").selectAll("g");

    thisGraph.drag = d3.behavior.drag()
          .origin(function(d){
            return {x: d.x, y: d.y};
          })
          .on("drag", function(args){
            thisGraph.state.justDragged = true;
            thisGraph.dragmove.call(thisGraph, args);
          })
          .on("dragend", function() {
            // todo check if edge-mode is selected
          });

    // listen for key events
    d3.select(window).on("keydown", function(){
      thisGraph.svgKeyDown.call(thisGraph);
    })
    .on("keyup", function(){
      thisGraph.svgKeyUp.call(thisGraph);
    });
    svg.on("mousedown", function(d){thisGraph.svgMouseDown.call(thisGraph, d);});
    svg.on("mouseup", function(d){thisGraph.svgMouseUp.call(thisGraph, d);});

    // listen for resize
    window.onresize = function(){thisGraph.updateWindow(svg);};

  };

  GraphCreator.prototype.setIdCt = function(idct){
    this.idct = idct;
  };

  GraphCreator.prototype.consts =  {
    selectedClass: "selected",
    connectClass: "connect-node",
    circleGClass: "conceptG",
    graphClass: "graph",
    activeEditId: "active-editing",
    BACKSPACE_KEY: 8,
    DELETE_KEY: 46,
    ENTER_KEY: 13,
    nodeRadius: 50
  };

  /* PROTOTYPE FUNCTIONS */

  GraphCreator.prototype.dragmove = function(d) {
    var thisGraph = this;
    if (thisGraph.state.shiftNodeDrag){
      thisGraph.dragLine.attr('d', 'M' + d.x + ',' + d.y + 'L' + d3.mouse(thisGraph.svgG.node())[0] + ',' + d3.mouse(this.svgG.node())[1]);
    } else{
      d.x += d3.event.dx;
      d.y +=  d3.event.dy;
      thisGraph.updateGraph();
    }
  };

  GraphCreator.prototype.deleteGraph = function(skipPrompt){
    var thisGraph = this,
        doDelete = true;
    if (!skipPrompt){
      doDelete = window.confirm("Press OK to delete this graph");
    }
    if(doDelete){
      thisGraph.nodes = [];
      thisGraph.edges = [];
      thisGraph.updateGraph();
    }
  };

  /* select all text in element: taken from http://stackoverflow.com/questions/6139107/programatically-select-text-in-a-contenteditable-html-element */
  GraphCreator.prototype.selectElementContents = function(el) {
    var range = document.createRange();
    range.selectNodeContents(el);
    var sel = window.getSelection();
    sel.removeAllRanges();
    sel.addRange(range);
  };


  /* insert svg line breaks: taken from http://stackoverflow.com/questions/13241475/how-do-i-include-newlines-in-labels-in-d3-charts */
  GraphCreator.prototype.insertTitleLinebreaks = function (gEl, title) {
    var words = title.split(/\s+/g),
        nwords = words.length;
    var el = gEl.append("text")
          .attr("text-anchor","middle")
          .attr("dy", "-" + (nwords-1)*7.5);

    for (var i = 0; i < words.length; i++) {
      var tspan = el.append('tspan').text(words[i]);
      if (i > 0)
        tspan.attr('x', 0).attr('dy', '15');
    }
  };


  // remove edges associated with a node
  GraphCreator.prototype.spliceLinksForNode = function(node) {
    var thisGraph = this,
        toSplice = thisGraph.edges.filter(function(l) {
      return (l.source === node || l.target === node);
    });
    toSplice.map(function(l) {
      thisGraph.edges.splice(thisGraph.edges.indexOf(l), 1);
    });
  };

  GraphCreator.prototype.replaceSelectEdge = function(d3Path, edgeData){
    var thisGraph = this;
    d3Path.classed(thisGraph.consts.selectedClass, true);
    if (thisGraph.state.selectedEdge){
      thisGraph.removeSelectFromEdge();
    }
    thisGraph.state.selectedEdge = edgeData;
  };

  GraphCreator.prototype.replaceSelectNode = function(d3Node, nodeData){
    var thisGraph = this;
    d3Node.classed(this.consts.selectedClass, true);
    if (thisGraph.state.selectedNode){
      thisGraph.removeSelectFromNode();
    }
    thisGraph.state.selectedNode = nodeData;
  };

  GraphCreator.prototype.removeSelectFromNode = function(){
    var thisGraph = this;
    thisGraph.circles.filter(function(cd){
      return cd.id === thisGraph.state.selectedNode.id;
    }).classed(thisGraph.consts.selectedClass, false);
    thisGraph.state.selectedNode = null;
  };

  GraphCreator.prototype.removeSelectFromEdge = function(){
    var thisGraph = this;
    thisGraph.paths.filter(function(cd){
      return cd === thisGraph.state.selectedEdge;
    }).classed(thisGraph.consts.selectedClass, false);
    thisGraph.state.selectedEdge = null;
  };

  GraphCreator.prototype.pathMouseDown = function(d3path, d){
    var thisGraph = this,
        state = thisGraph.state;
    d3.event.stopPropagation();
    state.mouseDownLink = d;

    if (state.selectedNode){
      thisGraph.removeSelectFromNode();
    }

    var prevEdge = state.selectedEdge;
    if (!prevEdge || prevEdge !== d){
      thisGraph.replaceSelectEdge(d3path, d);
    } else{
      thisGraph.removeSelectFromEdge();
    }
  };

  // mousedown on node
  GraphCreator.prototype.circleMouseDown = function(d3node, d){
    var thisGraph = this,
        state = thisGraph.state;
    d3.event.stopPropagation();
    state.mouseDownNode = d;
    if (d3.event.shiftKey){
      state.shiftNodeDrag = d3.event.shiftKey;
      // reposition dragged directed edge
      thisGraph.dragLine.classed('hidden', false)
        .attr('d', 'M' + d.x + ',' + d.y + 'L' + d.x + ',' + d.y);
      return;
    }
  };

  /* place editable text on node in place of svg text */
  GraphCreator.prototype.changeTextOfNode = function(d3node, d){
    var thisGraph= this,
        consts = thisGraph.consts,
        htmlEl = d3node.node();
    d3node.selectAll("text").remove();
    var nodeBCR = htmlEl.getBoundingClientRect(),
        curScale = nodeBCR.width/consts.nodeRadius,
        placePad  =  5*curScale,
        useHW = curScale > 1 ? nodeBCR.width*0.71 : consts.nodeRadius*1.42;
    // replace with editableconent text
    
    var d3txt = thisGraph.svg.selectAll("foreignObject")
          .data([d])
          .enter()
          .append("foreignObject")
          .attr("x", d.x - 2*placePad)
          .attr("y", d.y - placePad)
          .attr("height", 2*useHW)
          .attr("width", useHW)
          .append("xhtml:p")
          .attr("id", consts.activeEditId)
          .attr("contentEditable", "true")
          .text(d.title)
          .on("mousedown", function(d){
            d3.event.stopPropagation();
          })
          .on("keydown", function(d){
            d3.event.stopPropagation();
            if (d3.event.keyCode == consts.ENTER_KEY && !d3.event.shiftKey){
              this.blur();
            }
          })
          .on("blur", function(d){
            d.title = this.textContent;
            d.latent = 0;
            d.outcome = 0;
            d.exposure = 0;
            d.nvals = 2;
            thisGraph.insertTitleLinebreaks(d3node, d.title);
            d3.select(this.parentElement).remove();
          });
    return d3txt;
  };

  // mouseup on nodes
  GraphCreator.prototype.circleMouseUp = function(d3node, d){
    var thisGraph = this,
        state = thisGraph.state,
        consts = thisGraph.consts;
    // reset the states
    state.shiftNodeDrag = false;
    d3node.classed(consts.connectClass, false);

    var mouseDownNode = state.mouseDownNode;

    if (!mouseDownNode) return;

    thisGraph.dragLine.classed("hidden", true);

    if (mouseDownNode !== d){
      // we're in a different node: create new edge for mousedown edge and add to graph
      var newEdge = {source: mouseDownNode, target: d, monotone: 0};
      var filtRes = thisGraph.paths.filter(function(d){
        if (d.source === newEdge.target && d.target === newEdge.source){
          thisGraph.edges.splice(thisGraph.edges.indexOf(d), 1);
        }
        return d.source === newEdge.source && d.target === newEdge.target;
      });
      if (!filtRes[0].length){
        thisGraph.edges.push(newEdge);
        thisGraph.updateGraph();
      }
    } else{
      // we're in the same node
      if (state.justDragged) {
        // dragged, not clicked
        state.justDragged = false;
      } else{
        // clicked, not dragged
        if (d3.event.shiftKey){
          // shift-clicked node: edit text content
          var d3txt = thisGraph.changeTextOfNode(d3node, d);
          var txtNode = d3txt.node();
          thisGraph.selectElementContents(txtNode);
          txtNode.focus();
        } else{
          if (state.selectedEdge){
            thisGraph.removeSelectFromEdge();
          }
          var prevNode = state.selectedNode;

          if (!prevNode || prevNode.id !== d.id){
            thisGraph.replaceSelectNode(d3node, d);
          } else{
            thisGraph.removeSelectFromNode();
          }
        }
      }
    }
    state.mouseDownNode = null;
    return;

  }; // end of circles mouseup

  // mousedown on main svg
  GraphCreator.prototype.svgMouseDown = function(){
    this.state.graphMouseDown = true;
  };

  // mouseup on main svg
  GraphCreator.prototype.svgMouseUp = function(){
    var thisGraph = this,
        state = thisGraph.state;
    if (state.justScaleTransGraph) {
      // dragged not clicked
      state.justScaleTransGraph = false;
    } else if (state.graphMouseDown && d3.event.shiftKey){
      // clicked not dragged from svg
      var xycoords = d3.mouse(thisGraph.svgG.node()),
          d = {id: thisGraph.idct++, title: consts.defaultTitle, x: xycoords[0], y: xycoords[1]};
      thisGraph.nodes.push(d);
      thisGraph.updateGraph();
      // make title of text immediently editable
      var d3txt = thisGraph.changeTextOfNode(thisGraph.circles.filter(function(dval){
        return dval.id === d.id;
      }), d),
          txtNode = d3txt.node();
      thisGraph.selectElementContents(txtNode);
      txtNode.focus();
    } else if (state.shiftNodeDrag){
      // dragged from node
      state.shiftNodeDrag = false;
      thisGraph.dragLine.classed("hidden", true);
    }
    state.graphMouseDown = false;
  };

  // keydown on main svg
  GraphCreator.prototype.svgKeyDown = function() {
    var thisGraph = this,
        state = thisGraph.state,
        consts = thisGraph.consts;
    // make sure repeated key presses don't register for each keydown
    if(state.lastKeyDown !== -1) return;

    state.lastKeyDown = d3.event.keyCode;
    var selectedNode = state.selectedNode,
        selectedEdge = state.selectedEdge;
    
    switch(d3.event.keyCode) {
    case 68:
      d3.event.preventDefault();
      if (selectedNode){
        thisGraph.nodes.splice(thisGraph.nodes.indexOf(selectedNode), 1);
        thisGraph.spliceLinksForNode(selectedNode);
        state.selectedNode = null;
        thisGraph.updateGraph();
      } else if (selectedEdge){
        thisGraph.edges.splice(thisGraph.edges.indexOf(selectedEdge), 1);
        state.selectedEdge = null;
        thisGraph.updateGraph();
      }
      break;
    case 85: // u key for unobserved
        if (selectedNode) {
            state.selectedNode.latent = 1 - state.selectedNode.latent;
         //   if(state.selectedNode.latent == 1) {
          //      state.selectedNode.outcome = 0;
        //    }
            thisGraph.updateGraph();
        }
      break;
    case 89: // y key for outcome
        if (selectedNode) {
            console.log(d3.event.keyCode);
            var i;
            for (i = 0; i < thisGraph.nodes.length; i++) { 
                thisGraph.nodes[i].outcome = 0;
            }
            state.selectedNode.outcome = 1 - state.selectedNode.outcome;
       //     state.selectedNode.latent = 0;
            thisGraph.updateGraph();
        }
        break;
    case 69: // e key for exposure
        if (selectedNode) {
            console.log(d3.event.keyCode);
            var i;
            for (i = 0; i < thisGraph.nodes.length; i++) { 
                thisGraph.nodes[i].exposure = 0;
            }
            state.selectedNode.exposure = 1 - state.selectedNode.exposure;
            state.selectedNode.latent = 0;
            thisGraph.updateGraph();
        }
        break;
    case 67: // c key for cardinality/categories
        if (selectedNode) {
            console.log(d3.event.keyCode);
            $(document).ready(function() {
                $('#nvalsModal').modal();
                $('#nvalsSubmit').click(function() {
                    var nvalsNumber = $('#nvalsInput').val();
                    state.selectedNode.nvals = nvalsNumber;
                    toastMessage("Number of values of " + selectedNode.title + " is set to " + nvalsNumber);
                    thisGraph.updateGraph();
                });
            });
        }
        break;
    case 50: // 2 key for dichotomous variable
        if (selectedNode) {
            console.log(d3.event.keyCode);
            state.selectedNode.nvals = 2;
            toastMessage("Number of values of " + selectedNode.title + " is set to 2");
            thisGraph.updateGraph();
        }
        break;
    case 51: // 3 key for trichotomous variable
        if (selectedNode) {
            console.log(d3.event.keyCode);
            state.selectedNode.nvals = 3;
            toastMessage("Number of values of " + selectedNode.title + " is set to 3");
            thisGraph.updateGraph();
        }
        break;
    case 52: // 4 key for 4-level variable
        if (selectedNode) {
            console.log(d3.event.keyCode);
            state.selectedNode.nvals = 4;
            toastMessage("Number of values of " + selectedNode.title + " is set to 4");
            thisGraph.updateGraph();
        }
        break;
    case 53: // 5 key for 5-level variable
        if (selectedNode) {
            console.log(d3.event.keyCode);
            state.selectedNode.nvals = 5;
            toastMessage("Number of values of " + selectedNode.title + " is set to 5");
            thisGraph.updateGraph();
        }
        break;
    case 54: // 6 key for 6-level variable
        if (selectedNode) {
            console.log(d3.event.keyCode);
            state.selectedNode.nvals = 6;
            toastMessage("Number of values of " + selectedNode.title + " is set to 6");
            thisGraph.updateGraph();
        }
        break;
    case 55: // 7 key for 7-level variable
        if (selectedNode) {
            console.log(d3.event.keyCode);
            state.selectedNode.nvals = 7;
            toastMessage("Number of values of " + selectedNode.title + " is set to 7");
            thisGraph.updateGraph();
        }
        break;
    case 56: // 8 key for 8-level variable
        if (selectedNode) {
            console.log(d3.event.keyCode);
            state.selectedNode.nvals = 8;
            toastMessage("Number of values of " + selectedNode.title + " is set to 8");
            thisGraph.updateGraph();
        }
        break;
    case 57: // 9 key for 9-level variable
        if (selectedNode) {
            console.log(d3.event.keyCode);
            state.selectedNode.nvals = 9;
            toastMessage("Number of values of " + selectedNode.title + " is set to 9");
            thisGraph.updateGraph();
        }
        break;
     case 77: // monotone edge
        if (selectedEdge) {
            state.selectedEdge.monotone =  1 - state.selectedEdge.monotone;
            thisGraph.updateGraph();
        }
        break;
    }
  };

  GraphCreator.prototype.svgKeyUp = function() {
    this.state.lastKeyDown = -1;
  };

  // call to propagate changes to graph
  GraphCreator.prototype.updateGraph = function(){

    var thisGraph = this,
        consts = thisGraph.consts,
        state = thisGraph.state;

    thisGraph.paths = thisGraph.paths.data(thisGraph.edges, function(d){
      return String(d.source.id) + "+" + String(d.target.id);
    });
    var paths = thisGraph.paths;
    // update existing paths
    paths.style('marker-end', 'url(#end-arrow)')
      .classed(consts.selectedClass, function(d){
        return d === state.selectedEdge;
      })
      .attr("d", function(d){
        return "M" + d.source.x + "," + d.source.y + "L" + d.target.x + "," + d.target.y;
      });

    // add new paths
    paths.enter()
      .append("path")
      .style('marker-end','url(#end-arrow)')
      .classed("link", true)
      .attr("d", function(d){
        return "M" + d.source.x + "," + d.source.y + "L" + d.target.x + "," + d.target.y;
      })
      .on("mousedown", function(d){
        thisGraph.pathMouseDown.call(thisGraph, d3.select(this), d);
        }
      )
      .on("mouseup", function(d){
        state.mouseDownLink = null;
      });

    // remove old links
    paths.exit().remove();
    
    paths.each(function(d){ 
        if(d.monotone == 1) {
            this.classList.add("monotone");
        }
        if(d.monotone == 0) {
            this.classList.remove("monotone");
        }
    });

    // update existing nodes
    thisGraph.circles = thisGraph.circles.data(thisGraph.nodes, function(d){ return d.id;});
    thisGraph.circles.attr("transform", function(d){return "translate(" + d.x + "," + d.y + ")";});

    // add new nodes
    var newGs= thisGraph.circles.enter()
          .append("g");

    newGs.classed(consts.circleGClass, true)
      .attr("transform", function(d){return "translate(" + d.x + "," + d.y + ")";})
      .on("mouseover", function(d){
        if (state.shiftNodeDrag){
          d3.select(this).classed(consts.connectClass, true);
        }
      })
      .on("mouseout", function(d){
        d3.select(this).classed(consts.connectClass, false);
      })
      .on("mousedown", function(d){
        thisGraph.circleMouseDown.call(thisGraph, d3.select(this), d);
      })
      .on("mouseup", function(d){
        thisGraph.circleMouseUp.call(thisGraph, d3.select(this), d);
      })
      .call(thisGraph.drag);
    
   
    newGs.append("circle")
      .attr("r", String(consts.nodeRadius));

    newGs.each(function(d){
      thisGraph.insertTitleLinebreaks(d3.select(this), d.title);
      
    });
    
    thisGraph.circles.each(function(d){ 
        if(d.latent == 1) {
            this.classList.add("latent");
        }
        if(d.latent == 0) {
            this.classList.remove("latent");
        }
    });
    
    thisGraph.circles.each(function(d){ 
        if(d.outcome == 1) {
            this.classList.add("outcome");
        }
        if(d.outcome == 0) {
            this.classList.remove("outcome");
        }
    });
    
      thisGraph.circles.each(function(d){ 
        if(d.exposure == 1) {
            this.classList.add("exposure");
        }
        if(d.exposure == 0) {
            this.classList.remove("exposure");
        }
    });
    
      thisGraph.circles.each(function(d){ 
        if(d.nvals == 2) {
            this.classList.add("binary");
        }
        if(d.nvals > 2) {
            this.classList.add("multilevel");
        }
    });

    // remove old nodes
    thisGraph.circles.exit().remove();
    
     var saveEdges = [];
      thisGraph.edges.forEach(function(val, i){
          var width = svg.attr("width");
        saveEdges.push({id: val.source.id + '.' + val.target.id, source: val.source.title, target: val.target.title, 
            sourceLeftside: val.source.x < width / 2, targetLeftside: val.target.x < width / 2,
            rlconnect: (val.target.x < width / 2 && val.source.x > width / 2), 
            sourceLatent: val.source.latent, targetLatent: val.target.latent, 
            sourceOutcome: val.source.outcome, targetOutcome: val.target.outcome, 
            sourceExposure: val.source.exposure, targetExposure: val.target.exposure,
            sourceNvals: val.source.nvals, targetNvals: val.target.nvals,
            edgeMonotone: val.monotone, sourceX: val.source.x, sourceY: val.source.y, 
            targetX: val.target.x, targetY: val.target.y
        });
      });
    
    Shiny.setInputValue("edges", saveEdges)
  
    
  };

  GraphCreator.prototype.zoomed = function(){
    this.state.justScaleTransGraph = true;
    d3.select("." + this.consts.graphClass)
      .attr("transform", "translate(" + d3.event.translate + ") scale(" + d3.event.scale + ")");
  };

  GraphCreator.prototype.updateWindow = function(svg){
    var docEl = document.documentElement,
        bodyEl = document.getElementById('graph');
    var x = window.innerWidth || docEl.clientWidth || bodyEl.clientWidth;
    var y = bodyEl.clientHeight;
    svg.attr("width", x).attr("height", y);
    outlineL.attr("width", x / 2).attr("height", y);
    outlineR.attr("x", x / 2).attr("width", x / 2 - 10).attr("height", y);
  };



  /**** MAIN ****/

  // warn the user when leaving
//  window.onbeforeunload = function(){
 //   return "Make sure to save your graph locally before leaving :-)";
 // };

  var docEl = document.documentElement,
      bodyEl = document.getElementById('graph');

  var width = window.innerWidth || docEl.clientWidth || bodyEl.clientWidth,
      height =  bodyEl.clientHeight;

  var xLoc = width/2 - 25,
      yLoc = 100;

  // initial node data
  var nodes = [];
  var edges = [];


  /** MAIN SVG **/
  var svg = d3.select("#graph").append("svg")
        .attr("width", width)
        .attr("height", height);
  var outlineL = svg.append("rect")
                             .attr("x", 0)
                             .attr("y", 0)
                            .attr("width", width / 2)
                            .attr("height", height)
                            .attr("style", "fill:blue;stroke:pink;stroke-width:5;fill-opacity:0.1;stroke-opacity:0.9");
  var outlineR = svg.append("rect")
                             .attr("x", width / 2)
                             .attr("y", 0)
                            .attr("width", width / 2 - 20)
                            .attr("height", height)
                            .attr("style", "fill:yellow;stroke:green;stroke-width:5;fill-opacity:0.1;stroke-opacity:0.9");                        
  var graph = new GraphCreator(svg, nodes, edges);
      graph.setIdCt(2);
  graph.updateGraph();
 
 
 
//})(window.d3, window.saveAs, window.Blob);

function toastMessage(textMessage) {
    var x = document.getElementById("toast");
    x.className = "show";
    $('#toast').text(textMessage);
    setTimeout(function(){ x.className = x.className.replace("show", ""); }, 3000);
}
