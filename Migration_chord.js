
// Set up the basic chordplot dimensions
var screenWidth = window.innerWidth;

var margin = {left: 20, top: 20, right: 20, bottom: 20},
		width = Math.min(screenWidth, 600) - margin.left - margin.right, // This says if the screen width is less than 500 then use the screen width
		height = Math.min(screenWidth, 600) - margin.top - margin.bottom;

var innerRadius = Math.min(width, height) * .39;
var outerRadius = innerRadius * 1.04;

// Set up the Data
// create the ajax request to grab the source JSON data
var request = new XMLHttpRequest();
request.open("GET", "./Matrix_reg_migration.json", false);
request.send(null);

var matrix = JSON.parse(request.responseText); // parse the fetched json data into a variable

var NameProvider = ["North East","North West", "Yorkshire and The Humber", "East Midlands","West Midlands","East of England", "London", "South East", "South West", "Wales", "Scotland", "Northern Ireland"];
var	colors = ["#da3d5f","#c85b3d","#c58943","#b3b044","#5f7938","#63b54f","#4fb799","#5c92ce","#7066ca","#c085c6","#bd50b7","#bf577d"];
var	opacityDefault = 0.6;

console.table(matrix)

// var NameProvider = ["Black Widow","Captain America","Hawkeye","The Hulk","Iron Man","Thor"];
// var colors = ["#301E1E", "#083E77", "#342350", "#567235", "#8B161C", "#DF7C00"];
// var opacityDefault = 0.5;
//
// var matrix = [
// 	[0,4,3,2,5,2], //Black Widow
// 	[4,0,3,2,4,3], //Captain America
// 	[3,3,0,2,3,3], //Hawkeye
// 	[2,2,2,0,3,3], //The Hulk
// 	[5,4,3,3,0,2], //Rich Man
// 	[2,3,3,3,2,0], //Thor
// ];

// Create functions to show, move, and hide any tooltips
var tooltip = d3.select("#chart")
  .append("div")
  .attr("class", "tooltip")
  .style("position", "absolute")
  .style("z-index", "10")
  .style("visibility", "hidden");

// This creates the function for what to do when someone moves the mouse over a path (e.g. move the tooltip in relation to the mouse cursor).
var mousemove_path = function(d) {
    tooltip
      .html("<p>In 2018, there were <strong>" + 3 + "</strong> moves from " + NameProvider[d.source.index] + " to " + NameProvider[d.target.index] + " and <strong>" + 4 + "</strong> moves from " + NameProvider[d.target.index] + " to " + NameProvider[d.source.index] + ".</p>")
      .style("top", (event.pageY - 10) + "px")
      .style("left", (event.pageX + 10) + "px")
  }

// This creates the function for what to do when someone moves the mouse over a path (e.g. move the tooltip in relation to the mouse cursor).
var mousemove_chord = function(d) {
      tooltip
        .html("<p>The total moves out of " + NameProvider[d.index] + " were and the toal in were y. This means there was a net migration in 2018 of xy.</p>")
        .style("top", (event.pageY - 10) + "px")
        .style("left", (event.pageX + 10) + "px")
    }

/*Initiate the color scale*/
var colors = d3.scale.ordinal()
    .domain(d3.range(NameProvider.length)) // .length will give you the number of elements in the variable
    .range(colors);

var chord = d3.layout.chord()
      .padding(.05) // This is the spacing between bars
      .sortSubgroups(d3.descending) /*sort the chords inside an arc from high to low*/
      .sortChords(d3.descending) /*which chord should be shown on top when chords cross. Now the biggest chord is at the bottom*/
    	.matrix(matrix);

var arc = d3.svg.arc()
      .innerRadius(innerRadius)
      .outerRadius(outerRadius);

var path = d3.svg.chord()
  .radius(innerRadius);

// Initiate the SVG
var svg = d3.select("#chart").append("svg")
  .attr("width", width + margin.left + margin.right)
  .attr("height", height + margin.top + margin.bottom)
	.append("g")
  .attr("transform", "translate(" + (width / 2 + margin.left) + "," + (height / 2 + margin.top) + ")");

var outerArcs = svg.selectAll("g.group")
  	.data(chord.groups)
  	.enter().append("g")
  	.attr("class", "group")
  	.on("mouseover", fade(.1))
  	.on("mouseout", fade(opacityDefault));

outerArcs.append("path")
    .attr("id", function(d,i) { return "outerArcs_"+i; })
  	.style("fill", function(d) { return colors(d.index); })
  	.attr("d", arc)
    .on("mouseover", function() {
        return tooltip.style("visibility", "visible");
      })
    .on("mousemove", mousemove_chord)
    .on("mouseout", function() {
        return tooltip.style("visibility", "hidden");
      });

// Append the label names on the outside on the arc
outerArcs.append("text")
	.attr("class", "names")
	.attr("dy", -10)
  .append("textPath")
	.attr("startOffset","2%")
	.style("text-anchor","left")
	.attr("xlink:href",function(d,i){return "#outerArcs_"+i;})
	.text(function(d,i){ return NameProvider[i]; });

svg.selectAll("path.chord")
.data(chord.chords)
.enter().append("path")
.attr("class", "chord")
.style("fill", function(d) { return colors(d.source.index); })
.style("opacity", opacityDefault)
.attr("d", path)
.on("mouseover", function() {
    return tooltip.style("visibility", "visible");
  })
.on("mousemove", mousemove_path)
.on("mouseout", function() {
    return tooltip.style("visibility", "hidden");
  });

// Add the major ticks
outerArcs
    .selectAll(".group-tick")
    .data(function(d) { return groupTicks(d, 50); }) // Controls the number of ticks
    .enter()
    .append("g")
    .attr("transform", function(d) { return "rotate(" + (d.angle * 180 / Math.PI - 90) + ") translate(" + 200 + ",0)"; })
    .append("line")  // By default, x1 = y1 = y2 = 0, so no need to specify it.
    .attr("x2", .05)
    .attr("stroke", "black")

// Add the labels of a few ticks:
outerArcs
    .selectAll(".group-tick-label")
    .data(function(d) { return groupTicks(d, 50); })
    .enter()
    .filter(function(d) { return d.value % 0.5 === 0; })
    .append("g")
    .attr("transform", function(d) { return "rotate(" + (d.angle * 180 / Math.PI - 90) + ") translate(" + 200 + ",0)"; })
    .append("text")
    .attr("x", 8)
    .attr("dy", ".35em")
    .attr("transform", function(d) { return d.angle > Math.PI ? "rotate(180) translate(-16)" : null; })
    .style("text-anchor", function(d) { return d.angle > Math.PI ? "end" : null; })
    .text(function(d) { return d.value })
    .style("font-size", 8)

// You can also add in more ticks at a different interval and height as minor ticks
outerArcs
    .selectAll(".group-tick")
    .data(function(d) { return groupTicks(d, 10); })    // Controls the number of ticks: one tick each 1 percent here.
    .enter()
    .append("g")
    .attr("transform", function(d) { return "rotate(" + (d.angle * 180 / Math.PI - 90) + ") translate(" + 200 + ",0)"; })
    .append("line")               // By default, x1 = y1 = y2 = 0, so no need to specify it.
    .attr("x2", 2)
    .attr("stroke", "black")

// Returns an array of tick angles and values for a given group and step.
function groupTicks(d, step) {
  var k = (d.endAngle - d.startAngle) / d.value;
  return d3.range(0, d.value, step).map(function(value) {
  return {value: value, angle: value * k + d.startAngle};
  });
}

//Returns an event handler for fading a given chord group.
function fade(opacity) {
  return function(d,i) {
  svg.selectAll("path.chord")
  .filter(function(d) { return d.source.index != i && d.target.index != i; })
  .transition()
  .style("opacity", opacity);
  };
}

//Highlight hovered over chord
function mouseover(d,i) {
	svg.selectAll("path")
		.transition()
		.style("opacity", 0.1);
	d3.select(this)
		.transition()
        .style("opacity", 1);
}
