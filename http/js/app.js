/*

   NEPAL COMMUNITY SURVEY DASHBOARD: -------------------

   Here we visualize the Nepal community surveys available
   on the Humanitarian Data Exchange website:

   -----------------------------------------------------
   Author: Luis Capelo (capelo@un.org)
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

  STOP GAP: ------------------------------------

  Here we are mokey-patchin the DC.js original
  dataCount() function for allowing the aggregation
  of its displayed figure. Before rendering
  the figure on the DOM, this will not allow
  that number to below a certain arbitrary integer.

  ----------------------------------------------

*/
var LIMIT = 20
dc.dataCount = function (parent, chartGroup) {
  var _formatNumber = d3.format(',d')
  var _chart = dc.baseMixin({})
  var _html = {some: '', all: ''}
  var _checkAnonimityLimit = function (limit, figure) {
    if (parseInt(figure.replace(',', '')) > limit) {
      return figure
    } else {
      console.warn('Too low of integer detected. Activating stop gap.')
      return '< ' + limit + '*'
    }
  }

  _chart.html = function (options) {
    if (!arguments.length) {
      return _html
    }
    if (options.all) {
      _html.all = options.all
    }
    if (options.some) {
      _html.some = options.some
    }
    return _chart
  }

  _chart.formatNumber = function (formatter) {
    if (!arguments.length) {
      return _formatNumber
    }
    _formatNumber = formatter
    return _chart
  }

  _chart._doRender = function () {
    var tot = _chart.dimension().size(),
      val = _chart.group().value()
    var all = _formatNumber(tot)
    var selected = _formatNumber(val)
    selected = _checkAnonimityLimit(LIMIT, selected)

    if ((tot === val) && (_html.all !== '')) {
      _chart.root().html(_html.all.replace('%total-count', all).replace('%filter-count', selected))
    } else if (_html.some !== '') {
      _chart.root().html(_html.some.replace('%total-count', all).replace('%filter-count', selected))
    } else {
      _chart.selectAll('.total-count').text(all)
      _chart.selectAll('.filter-count').text(selected)
    }
    return _chart
  }

  _chart._doRedraw = function () {
    return _chart._doRender()
  }

  return _chart.anchor(parent, chartGroup)
}

var loadData = function () {
  queue()
    .defer(d3.json, 'http/data/data.json')
    .defer(d3.json, 'http/data/adm3.json')
    .defer(d3.csv, 'http/data/3w.csv')
    .await(function (error, data, district_data, data_3w) {
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
      var gender_chart = dc.pieChart('#gender_chart')
      var ethnicity_chart = dc.pieChart('#ethnicity_chart')
      var occupation_chart = dc.rowChart('#occupation_chart')

      // Questions
      var A0JS_chart = dc.rowChart('#A0JS')
      var A1JS_chart = dc.rowChart('#A1JS')
      var B0JS_chart = dc.rowChart('#B0JS')
      var C1JS_chart = dc.rowChart('#C1JS')
      var D0JS_chart = dc.rowChart('#D0JS')
      var E0JS_chart = dc.rowChart('#E0JS')

      // 3W
      var who_chart = dc.rowChart('#who')
      var what_chart = dc.rowChart('#what')

      /*

        CROSSFILTER OBJECTS: ----------------------

        This creates a Crossfilter object, which
        effectively allows for multi-dimensional
        filtering of other charts.

        -------------------------------------------

      */
      var cf = crossfilter(data)
      var cf_3w = crossfilter(data_3w)

      /*

        Declaring Crossfilter dimensions. This can
        also be used for light post-processing of data.
        For instance, one could calculate of the Crossfilter
        variables here.

      */
      // Demographics
      cf.age = cf.dimension(function (d) { return d.Age })
      cf.round = cf.dimension(function (d) { return d.Round })
      cf.gender = cf.dimension(function (d) { return d.Gender })
      cf.district = cf.dimension(function (d) { return d.District })
      cf.ethnicity = cf.dimension(function (d) { return d.Ethnicity })
      cf.occupation = cf.dimension(function (d) { return d.Occupation })

      // Questions
      cf.a0js = cf.dimension(function (d) { return d.A0JS })
      cf.a1js = cf.dimension(function (d) { return d.A1JS })
      cf.b0js = cf.dimension(function (d) { return d.B0JS })
      cf.c1js = cf.dimension(function (d) { return d.C1JS })
      cf.d0js = cf.dimension(function (d) { return d.D0JS })
      cf.e0js = cf.dimension(function (d) { return d.E0JS })

      // 3W data
      cf_3w.district = cf_3w.dimension(function (d) { return d.District })
      cf_3w.organization = cf_3w.dimension(function (d) { return d['Organization'] })
      cf_3w.cluster = cf_3w.dimension(function (d) { return d['Cluster'] })

      /*

        Declaring Crossfilter groups.

      */
      // Demographics
      var all = cf.groupAll()
      var ages = cf.age.group()
      var rounds = cf.round.group()
      var genders = cf.gender.group()
      var districts = cf.district.group()
      var ethnicities = cf.ethnicity.group()
      var occupations = cf.occupation.group()
      var total_per_district = cf.district.group()

      // Questions
      var question_a0js = cf.a0js.group()
      var question_a1js = cf.a1js.group()
      var question_b0js = cf.b0js.group()
      var question_c1js = cf.c1js.group()
      var question_d0js = cf.d0js.group()
      var question_e0js = cf.e0js.group()

      // 3W
      var districts_3w = cf_3w.district.group()
      var organizations = cf_3w.organization.group()
      var clusters = cf_3w.cluster.group()

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
        .colors(['#8C510A'])
        .colorAccessor(function (d, i) { return i })
        .margins({top: 20, left: 30, right: 30, bottom: 20})
        .dimension(cf.occupation)
        .group(occupations)
        .elasticX(true)
        .x(d3.scale.linear()).gap(1)
        .xAxis().ticks(5)

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
        .x(d3.scale.ordinal().domain(['15 to 24', '25 to 39', '40 to 54', '55+', "Don't know", 'Refused']))
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

      gender_chart
        .width(500).height(170)
        .dimension(cf.gender)
        .group(genders)
        .colors(['#d7191c', '#fdae61', '#ffffbf', '#abd9e9', '#2c7bb6'])
        .colorDomain([0, 5])
        .innerRadius(30)
        .legend(dc.legend().x(2).y(30).itemHeight(13).gap(5))
        .renderLabel(true)
        .renderTitle(false)
        .colorAccessor(function (d, i) { return i % 5 })

      dc.dataCount('#count-info')
        .dimension(cf)
        .group(all)

      // Questions
      A0JS_chart
        .width(300)
        .height(200)
        .colors(['#35978F'])
        .colorAccessor(function (d, i) { return i })
        .margins({top: 20, left: 30, right: 30, bottom: 20})
        .dimension(cf.a0js)
        .group(question_a0js)
        .elasticX(true)
        .x(d3.scale.linear()).gap(1)
        .xAxis().ticks(5)

      A1JS_chart
        .width(300)
        .height(200)
        .colors(['#35978F'])
        .colorAccessor(function (d, i) { return i })
        .margins({top: 20, left: 30, right: 30, bottom: 20})
        .dimension(cf.a1js)
        .group(question_a1js)
        .elasticX(true)
        .x(d3.scale.linear()).gap(1)
        .xAxis().ticks(5)

      B0JS_chart
        .width(300)
        .height(200)
        .colors(['#35978F'])
        .colorAccessor(function (d, i) { return i })
        .margins({top: 20, left: 30, right: 30, bottom: 20})
        .dimension(cf.b0js)
        .group(question_b0js)
        .elasticX(true)
        .x(d3.scale.linear()).gap(1)
        .xAxis().ticks(5)

      C1JS_chart
        .width(300)
        .height(200)
        .colors(['#35978F'])
        .colorAccessor(function (d, i) { return i })
        .margins({top: 20, left: 30, right: 30, bottom: 20})
        .dimension(cf.c1js)
        .group(question_c1js)
        .elasticX(true)
        .x(d3.scale.linear()).gap(1)
        .xAxis().ticks(5)

      D0JS_chart
        .width(300)
        .height(200)
        .colors(['#35978F'])
        .colorAccessor(function (d, i) { return i })
        .margins({top: 20, left: 30, right: 30, bottom: 20})
        .dimension(cf.d0js)
        .group(question_d0js)
        .elasticX(true)
        .x(d3.scale.linear()).gap(1)
        .xAxis().ticks(5)

      E0JS_chart
        .width(300)
        .height(200)
        .colors(['#35978F'])
        .colorAccessor(function (d, i) { return i })
        .margins({top: 20, left: 30, right: 30, bottom: 20})
        .dimension(cf.e0js)
        .group(question_e0js)
        .elasticX(true)
        .x(d3.scale.linear()).gap(1)
        .xAxis().ticks(5)

      // 3W
      who_chart
        .width(300)
        .height(200)
        .colors(['#379adc'])
        .colorAccessor(function (d, i) { return i })
        .dimension(cf_3w.organization)
        .group(organizations)
        .elasticX(true)
        .data(function (d) {
          return d.top(15)
        })
        .xAxis().ticks(5)

      what_chart
        .width(300)
        .height(200)
        .dimension(cf_3w.cluster)
        .group(clusters)
        .elasticX(true)
        .data(function (d) {
          return d.top(15)
        })
        .colors(['#379adc'])
        .colorAccessor(function (d, i) { return i })
        .xAxis().ticks(5)

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
        .dimension(cf_3w.district)
        .group(total_per_district)
        .colors(d3.scale.ordinal().range(['#41ab5d', '#74c476', '#a1d99b', '#c7e9c0', '#e5f5e0', '#f7fcf5']))
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

        function tooltipText (d) {
          return '<b>' + d.properties.DISTRICT + '</b>'
        }
        d3.selectAll('.DISTRICT')
          .call(d3.helper.tooltip(function (d, i) { return tooltipText(d) }))

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
