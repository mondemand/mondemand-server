The mondemand server listens on one or more lwes channels for mondemand events
then routes those events to different backends.  Included backends mostly
write to files.  Additional backends are in different repositories.

Understanding Aggregation
=========================

This explanation will assume that rrds are used as the backend along
with mondemand-backend-stats-rrd.

Gauges and Counters
-------------------

There are two types of metrics provided by rrd: counters and gauges.

Gauges save the exact value as given, no divisions nor any other
calculations are performed.

Counters are always a rate of change over a period of time, which in
the case of our monitoring system is always 60 seconds. This is
essentially dividing the given value by 60, which is the period for
the monitoring system. In order to see the exact values given, the
series can be scaled by 60.

Namespaces
----------

There are two namespaces in the monitoring system: agg and md. All
automatic aggregations are in the 'agg' namespace. They are all of the
gauge type as they store a sum (or min or max or count) for each
minute.

All standard data is in the 'md' namespace.

Types of Aggregations: Automatic and in-browser
-----------------------------------------------

The automatic aggregations in the 'agg' namespace are the equivalent
of running sumSeries on a series of data. The difference is that for
sumSeries, all the data for the matching contexts has to be loaded
into the browser, after which the sum is calculated. The automatic
aggregations are automatically calculated on the servers and provide a
significant increase in speed to calculating the sumSeries in the
browser.

If your standard data (data in the 'md' namespace) is of a gauge type
as well, then performing a sumSeries of all the associated contexts of
a metric will result in an equivalent graph as that provided by the
automatic aggregations.

If your standard data is of the counter type and you perform a
sumSeries across all of the associated contexts for that metric, you
will still have a rate of change for the summed values. This will not
be the equivalent to the automatically aggregated data, as it is only
storing the sum for each minute. In order to get the series of values
to match, you will either need to scale by 60 the summed standard data
in order to get the exact values at those times, or scale the
automatically aggregated data by 1/60th (which is what the Graphite
function scaleToSeconds(1) does) in order to get the rate of change.

Which method you use depends on what you want, but typically, if you
are using counters in the standard data (looking for the rate of
change), then it makes sense to adjust the automatically aggregated
data to a rate of change as well.

Correctly Scaling For Separate Time Ranges
------------------------------------------

Recall that there are three archives that store values for various
periods of time:

* 31 days of 1 minute samples
* 100 days of 15 minute intervals
* 400 days of 1 day intervals

This means that when using the scaleToSeconds function provided by
graphite, care will need to be taken in order to scale by the
appropriate amount for the specified interval. For a window spanning
the last:

* 30 days or less (60 second interval), scale by 1/60th or use scaleToSeconds(1)
* 31-100 days (900 second interval), scale by 1/900th or use scaleToSeconds(15)
* older than 100 days (86400 second interval), scale by 1/86400 or use scaleToSeconds(1440)

The values given to scaleToSeconds() are determined by considering
that scaleToSeconds() is already dividing by 60 for scaleToSeconds(1).
Thus in order to scale a series by the correct factor for a 900 second (15 minute)
interval, use 900/60 = 15 in the scaleToSeconds function. For scaling
a series for data older than 100 days, it is necessary to use
scaleToSeconds(1440) as 86400/60=1440.
