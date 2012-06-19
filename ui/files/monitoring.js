

/*
 * 
var monitoroverride = websocket.onmessage;

websocket.onmessage = function (temp) {
	monitoroverride();	
	switch(msg.action) {
		case "newitem":
			switch(msg.item) {	
				case "chart":	
						var response = AddChart(msg.id, msg.type, msg.title, msg.subtitle);
							var responsemsg = {
								id:   response
							  };
							//websocket.send(JSON.stringify(responsemsg));
							break;			
			}
			break;
		case "updateitem":
			switch(msg.item) {
				case "chart":
					var response = AddData(msg.id, msg.data);
					var responsemsg = {
						id: response
					};
					//websocket.send(JSON.stringify(responsemsg));
					break;
			}
			break;
	}
}
*/

var diagrams = [];
var layouters = [];
var renderers = [];

function AddDiagram(id, width, height) {
	var newdiv = document.createElement('div'); 
	newdiv.setAttribute('id', id);
	var pos = document.getElementById("diagramloc");
	pos.appendChild(newdiv);
	var g = new Graph();
	var layouter = new Graph.Layout.Spring(g);
	var renderer = new Graph.Renderer.Raphael(id, g, width, height);
	diagrams[id] = g;
	layouters[id] = layouter;
	renderers[id] = renderer;

}


function AddDiagramNode(diagramid, nodeid, label) {
	var graph = diagrams[diagramid];
	graph.addNode(nodeid, { label : label });
	layouters[diagramid].layout();
        renderers[diagramid].draw();
}

function AddDiagramEdge(diagramid, nodeid1, nodeid2, directed) {
	var graph = diagrams[diagramid];
	if (directed) {
	    graph.addEdge(nodeid1, nodeid2, { directed : directed } );
	
	}
	else {
	    graph.addEdge(nodeid1, nodeid2);
	}
	layouters[diagramid].layout();
        renderers[diagramid].draw();
}

function AddData(chartid, datapoint) {
	var chart = charts[chartid];
	var series = chart.series[0];	
	shift = series.data.length > 20;	
	chart.series[0].addPoint(datapoint, true, shift);
}

var charts = [];

function AddChart(id, ctype, title, subtitle, width, height) {

//let's create a new div for the chart
var newdiv = document.createElement('div'); 
newdiv.setAttribute('id', id);
newdiv.style.height = height;
newdiv.style.width = width;
newdiv.setAttribute("style", "min-width: " + width.toString() + "px");
newdiv.setAttribute("style", "margin: 0 auto");

var pos = document.getElementById("chartloc");
pos.appendChild(newdiv);

var chart;

    $(document).ready(function() {
        chart = new Highcharts.Chart({
            chart: {             
				renderTo: id,
                type: ctype,
                //margin: [70, 50, 60, 80],
                events: {
			
						/*load: function() {
							
							setInterval(function() {
                            var x = (new Date()).getTime(), //current time
                                y = Math.random();
                            series.addPoint([x, y], true, true);
                        }, interval * 1000);
						}*/
				
					/*click: function(e) {
                        // find the clicked values and the series
                        var x = e.xAxis[0].value,
                            y = e.yAxis[0].value,
                            series = this.series[0];

                        // Add it
                        series.addPoint([x, y]);

                    }*/
                }
            },
            title: {
                text: title
            },
            subtitle: {
                text: subtitle
            },
            xAxis: {
				type: 'datetime',
				tickPixelInterval: 150,
                minPadding: 0.2,
                maxPadding: 0.2,
                maxZoom: 20 * 1000
            },
            yAxis: {
                title: {
                    text: 'Value'
                },
                minPadding: 0.2,
                maxPadding: 0.2,
                plotLines: [{
                    value: 0,
                    width: 1,
                    color: '#808080'
                }]
            },
            legend: {
                enabled: false
            },
            exporting: {
                enabled: false
            },
            plotOptions: {
                series: {
                    lineWidth: 1,
                    point: {
                        events: {
                          /*  'click': function() {
                                if (this.series.data.length > 1) this.remove();
                            }*/
                        }
                    }
                }
            },
            series: [{
                data: []
            }]
        });
    });
	charts[id] = chart;
}


