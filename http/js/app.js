/*

   FLOWMINDER NEPAL: -----------------------------------

   Example visualization designed for training purposes.
   This visualization represents flows above normal (for
   August) and the population still away from their
   original location.

   -----------------------------------------------------
   Author: Luis Capelo (luis.capelo@flowminder.org)
   -----------------------------------------------------

*/

/*

  Helper function; aids in printing
  cross filter dimensions and exploring
  if something is wrong.

*/
function print_filter (filter) {
  var f = eval(filter)
  if (typeof (f.length) != 'undefined') {} else {}
  if (typeof (f.top) != 'undefined') {f = f.top(Infinity);} else {}
  if (typeof (f.dimension) != 'undefined') {f = f.dimension(function (d) { return '';}).top(Infinity);} else {}
  console.log(filter + '(' + f.length + ') = ' + JSON.stringify(f).replace('[', '[\n\t').replace(/}\,/g, '},\n\t').replace(']', '\n]'))
}

/*

  DATA LOADING: -----------------------------

  Here we load heavy data files asynchronously
  using queue.js.

  -------------------------------------------

*/
var loadData = function () {
  queue()
    .defer(d3.json, 'http/data/data.json')
    .defer(d3.json, 'http/data/adm3.json')
    .await(function (error, data, district_data) {
      if (error) {
        throw error
      }
      /*

        CHART ELEMENTS: ---------------------------

        This section helps define all the charting
        elements. In this example we are only using
        barChart and a geoChoroplethChart. Also, we
        will be using some of those to represent
        overall figures.

        -------------------------------------------

      */
      // Demographics
      var map = dc.geoChoroplethChart('#map')
      var age_chart = dc.barChart('#age_chart')
      var ethnicity_chart = dc.pieChart('#ethnicity_chart')
      var occupation_chart = dc.rowChart('#occupation_chart')

      // Questions
      var A0JS_chart = dc.barChart('#A0JS')
      var A1JS_chart = dc.barChart('#A1JS')

      /*

        CROSSFILTER OBJECTS: ----------------------

        This creates a Crossfilter object, which
        effectively allows for multi-dimensional
        filtering of other charts.

        -------------------------------------------

      */
      var cf = crossfilter(data)

      /*

        Declaring Crossfilter dimensions. This can
        also be used for light post-processing of data.
        For instance, one could calculate of the Crossfilter
        variables here.

      */
      // Demographics
      cf.age = cf.dimension(function (d) { return d.Age })
      cf.round = cf.dimension(function (d) { return d.Round })
      cf.district = cf.dimension(function (d) { return d.District })
      cf.ethnicity = cf.dimension(function (d) { return d.Ethnicity })
      cf.occupation = cf.dimension(function (d) { return d.Occupation })

      // Questions
      cf.a0js = cf.dimension(function (d) { return d.A0JS })
      cf.a1js = cf.dimension(function (d) { return d.A1JS })

      /*

        Declaring Crossfilter groups.

      */
      // Demographics
      var all = cf.groupAll()
      var ages = cf.age.group()
      var rounds = cf.round.group()
      var districts = cf.district.group()
      var ethnicities = cf.ethnicity.group()
      var occupations = cf.occupation.group()
      var total_per_district = cf.district.group()

      // Questions
      var question_a0js = cf.a0js.group()
      var question_a1js = cf.a1js.group()

      /*

        CHARTING ELEMENTS: ---------------------------

        Here we construct the charts. We provided
        dimension parameters as well as rendering
        parameters.

        ----------------------------------------------

      */
      occupation_chart
        .width(300)
        .height(200)
        .colors(['#cccccc'])
        .margins({top: 20, left: 30, right: 30, bottom: 20})
        .dimension(cf.occupation)
        .group(occupations)
        .elasticX(true)
        .x(d3.scale.linear())
        .gap(1)

      age_chart
        .width(400)
        .height(250)
        .colors(['#e67e22'])
        .margins({top: 20, left: 60, right: 30, bottom: 60})
        .dimension(cf.age)
        .group(ages)
        .renderVerticalGridLines(true)
        .elasticY(true)
        .elasticX(false)
        .x(d3.scale.ordinal().domain(['15_24', '25_39', '40_54', '55', '55_greater', 'refused', 'don_t_know']))
        .xUnits(dc.units.ordinal)
        .xAxis().tickFormat()

      ethnicity_chart
        .width(500).height(170)
        .dimension(cf.ethnicity)
        .group(ethnicities)
        .colors(['#8c510a', '#bf812d', '#dfc27d', '#f6e8c3', '#f5f5f5', '#c7eae5', '#80cdc1', '#35978f', '#01665e'])
        .colorDomain([0, 5])
        .innerRadius(30)
        .legend(dc.legend().x(2).y(30).itemHeight(13).gap(5))
        .renderLabel(true)
        .renderTitle(false)
        .colorAccessor(function (d, i) { return i % 6 })

      dc.dataCount('#count-info')
        .dimension(cf)
        .group(all)

      A0JS_chart
        .width(400)
        .height(250)
        .colors(['#35978F'])
        .margins({top: 20, left: 60, right: 30, bottom: 60})
        .dimension(cf.a0js)
        .group(question_a0js)
        .renderVerticalGridLines(true)
        .elasticY(true)
        .elasticX(false)
        .x(d3.scale.ordinal().domain(['1___not_at_all', '2___very_little', '3___neutral', '4___mostly_yes', '5___completely_yes', 'don_t_know', 'refused']))
        .xUnits(dc.units.ordinal)
        .xAxis().tickFormat()

      A1JS_chart
        .width(400)
        .height(250)
        .colors(['#35978F'])
        .margins({top: 20, left: 60, right: 30, bottom: 60})
        .dimension(cf.a1js)
        .group(question_a1js)
        .renderVerticalGridLines(true)
        .elasticY(true)
        .elasticX(false)
        .x(d3.scale.ordinal().domain(['clean_water', 'financial_support', 'education', 'food', 'healthcare', 'livelihoods', 'housing_inspections', 'long_term_shelter__housing', 'other', 'psychosocial_counseling', 'seeds_and_fertilizers', 'short_term_shelter__tent_shelt', 'toilets_sanitation', 'short_term_she', '']))
        .xUnits(dc.units.ordinal)
        .xAxis().tickFormat()

      /*

          MAP: --------------------------------------

          Here we create the SVG map. To avoid not
          representing all districts, the map will
          fill with a grey tone all those in which
          no data is available.

          -------------------------------------------

      */
      var big_scale = 4300
      var big_center = [86.3, 27.6]
      map
        .width(650)
        .height(380)
        .dimension(cf.district)
        .group(total_per_district)
        .colors(d3.scale.ordinal().range(['#f7fcf5', '#e5f5e0', '#c7e9c0', '#a1d99b', '#74c476', '#41ab5d']))
        .colorDomain([d3.min(function (d) { return d.n }), d3.max(function (d) { return d.n })])
        .colorCalculator(function (d) { return d ? map.colors()(d) : '#ecf0f1' })
        .overlayGeoJson(district_data.features, 'DISTRICT', function (d) {
          return d.properties.DISTRICT
        })
        .projection(d3.geo.mercator().center(big_center).scale(big_scale))

      var addContour = function () {
        /*

          ADMIN 0 CONTOURS: ------------------------------

          Here we add a level 0 administrative boundary
          contour to our visualization.

            - map

          ------------------------------------------------

        */
        var big_scale = 4300
        var big_center = [86.3, 27.6]
        var projection = d3.geo.mercator()
          .center(big_center)
          .scale(big_scale)

        var path = d3.geo.path()
          .projection(projection)

        var g = d3.selectAll('#map')
          .select('svg')
          .append('g')

        g.selectAll('path')
          .data(adm0.features)
          .enter()
          .append('path')
          .attr('d', path)
          .attr('stroke', '#2c3e50')
          .attr('stroke-width', '2px')
          .attr('fill', 'none')
      }

      dc.renderAll()
      addContour()

      window.filterRound = function (selector) {
        console.log('Selecting round:', selector)
        cf.round.filterExact(selector)
        dc.redrawAll()
        addContour()
      }

      window.clearAllFilters = function () {
        cf.round.filterExact(undefined)
        dc.filterAll()
        dc.redrawAll()
      }

    })
}

loadData()
