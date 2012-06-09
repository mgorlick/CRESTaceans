
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
							websocket.send(JSON.stringify(responsemsg));
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
					websocket.send(JSON.stringify(responsemsg));
					break;
			}
			break;
	}
}

function AddData(chartid, datapoint) {
	var chart = charts[chartid];
	var series = chart.series[0];	
	shift = series.data.length > 20;	
	chart.series[0].addPoint(datapoint, true, shift);
}

var charts = [];

function AddChart(id, ctype, title, subtitle) {
var chart;

    $(document).ready(function() {
        chart = new Highcharts.Chart({
            chart: {             
				renderTo: 'chart',
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


