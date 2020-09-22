$(document).ready(function() {
  
  var t = setInterval(function(){
    /***
    Highcharts.charts.map(function(e) { e.reflow() });
    ***/
    
    window.allHighCharts = Highcharts.charts;
    
    for (var chartCnt = 0; chartCnt < allHighCharts.length; chartCnt++) {
      var chart = allHighCharts[chartCnt];
      if(chart){
        chart.reflow();
      }
    }
    
    
  }, 1000);
  
});