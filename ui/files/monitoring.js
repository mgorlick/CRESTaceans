alert("IM LOADED");
var monitoroverride = websocket.onmessage;


websocket.onmessage = function (temp) {
	monitoroverride();	
	switch(msg.action) {
		case "newitem":
			switch(msg.item) {	
				case "linegraph":	
						var response = AddChart(msg.id);
							var responsemsg = {
								id:   response
							  };
							websocket.send(JSON.stringify(responsemsg));
							break;			
			}
			break;
	}
}

function AddChart(id) {
var chart;
    $(document).ready(function() {
        chart = new Highcharts.Chart({
            chart: {
                renderTo: 'chart',
                type: 'line',
                margin: [70, 50, 60, 80],
                events: {
                    click: function(e) {
                        // find the clicked values and the series
                        var x = e.xAxis[0].value,
                            y = e.yAxis[0].value,
                            series = this.series[0];

                        // Add it
                        series.addPoint([x, y]);

                    }
                }
            },
            title: {
                text: 'Add Data Here'
            },
            subtitle: {
                text: 'Click the plot area to add a point. Click a point to remove it.'
            },
            xAxis: {
                minPadding: 0.2,
                maxPadding: 0.2,
                maxZoom: 60
            },
            yAxis: {
                title: {
                    text: 'Value'
                },
                minPadding: 0.2,
                maxPadding: 0.2,
                maxZoom: 60,
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
                            'click': function() {
                                if (this.series.data.length > 1) this.remove();
                            }
                        }
                    }
                }
            },
            series: [{
                data: [[20, 20], [80, 80]]
            }]
        });
    });

}


