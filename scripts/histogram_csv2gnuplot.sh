#!/bin/bash
# parameters to the script:
# Parameter1: frequency; default is: monthly
# Parameter2: generated gnuplot script name; default is: result.p
# Parameter3: generated result (pd) name; default is: result.pdf

function create_gnuplot_script()
{
# function declaration
# Parameter1 is the column of the X value for the graphs
# Parameter2 is the column of the Y value for the graphs
# Parameter3 is the Title of the graph
# Parameter4 is the name of the pdf file the result will be stored
# Parameter5 is format (pdf/X11)
# Parameter6 is the y label (default = "number of tickets")
# Parameter7 is the axis to be used (default = x1y1)
    
# echo "Parameter #1 is $1"
# echo "Parameter #2 is $2"
PDF_FILE=${4:-"result.pdf"} ;
FORMAT=${5:-"X11"} ;
YLABEL=${6:-"number of tickets"}
AXIS=${7:-"x1y1"} ;
TEMP="SUM_monthly.gnuplot"
GNUPLOT="$OUTPUT"
# gnuplot script creation
COUNT=`head -n1 "$TEMP" | wc -w | sed s/" "*//g`
# build output/format statement

if [ "$FORMAT" = "png" ]
then
   TERM="set terminal png size $WIDTH,$HEIGHT"
   OUT="set output \"$OUTPUT_PLOT\""
fi
if [ "$FORMAT" = "ps" ]
then
   TERM="set terminal postscript"
   OUT="set output \"$OUTPUT_PLOT\""
fi
if [ "$FORMAT" = "pdf" ]
then
   TERM="set terminal postscript  eps enhanced color solid lw 2 font 'Helvetica,12'"
    OUT="set output '| ps2pdf - temp_output.pdf'"      
fi
# build "with" statement
TMP=""
WITH=""
if [ "$LINES" = "yes" ]
then
   TMP=$TMP" lines"
fi
if [ "$TITLE" = "yes" ]
then
   TMP=$TMP" title #"
fi
if [ ! "$TMP" = "" ]
then
   WITH=" with"$TMP
fi
# init
echo "# gnuplot script for '$OUTPUT'" > "$GNUPLOT"
if [ ! "$GNUPLOT_OPTIONS" = "" ]
then
   cat $GNUPLOT_OPTIONS >> "$GNUPLOT"
fi
echo "set terminal postscript" >> "$GNUPLOT"
echo "set key left" >> "$GNUPLOT"
echo "set grid y" >> "$GNUPLOT"
echo "  set yrange [0 :*]" >> "$GNUPLOT"
echo "  set ylabel \"$YLABEL\" tc lt 1" >> "$GNUPLOT"
# echo "  set y2range [0 :*]" >> "$GNUPLOT"
# if [ $AXIS = "x1y2" ]
#   then 
#       echo "  set y2label 'no of comments' tc lt 2" >> "$GNUPLOT"
#       echo "  set y2range [0 :*]" >> "$GNUPLOT"
#   else
#       echo "  set ylabel 'no of tickets' tc lt 1" >> "$GNUPLOT"
# fi
echo "  set ytics nomirror" >> "$GNUPLOT"
echo "  set xtics nomirror rotate by -45 scale 0 font \",10\"" >> "$GNUPLOT"
echo "  set style data histograms" >> "$GNUPLOT"
echo "  set style histogram rowstacked" >> "$GNUPLOT"
echo "  set style fill solid border -1" >> "$GNUPLOT"
echo "  set boxwidth 0.75" >> "$GNUPLOT"
echo "  set key noinvert box" >> "$GNUPLOT"
echo "  set title \"$3\" " >> "$GNUPLOT"
echo "  rgb(r,g,b)=int(255*r)*65536+int(255*g)*256+int(255*b)" >>  "$GNUPLOT"
echo "  do for [i=1:31] { " >> "$GNUPLOT"
echo "      myrand=rand(int(rand(0)*i*100)+i*100) " >> "$GNUPLOT"
echo "      set style line i linecolor rgb rgb(rand(0),rand(0),rand(0)) " >> "$GNUPLOT"
echo "  } " >> "$GNUPLOT"

# the plots
i=0
ls -1 *_${F}.gnuplot | \
while read filename ; do
    let i++
    TMP='temp.tmp'
    title=`basename "$filename" .gnuplot`
    legend=`echo ${title%_monthly}`
    # legend=`echo $title | sed -e 's/_monthly$//'` 
    if [ $title == "SUM_monthly" ]
    then
	echo "SUM"
    else
	if [ $i == 1 ]
	then
	    echo "plot \"$filename\" using $2:xtic($1) title \"$legend\" noenhanced ls 1 " > $TMP
	else	   
 	    echo "replot \"$filename\" using $2:xtic($1) title \"$legend\" noenhanced ls $i " >> $TMP
	fi
    fi
    echo >> "$TMP"
done

echo "  set timefmt \"%Y/%m\" " >> "$GNUPLOT"
echo "  set x2data time" >> "$GNUPLOT"
echo "  set format x2 \"%Y/%m\" " >> "$GNUPLOT"
echo "  set x2range [\"2014/01\":\"2015/09\"]" >> "$GNUPLOT"
echo "  set y2label 'no of reported hours' tc lt 2" >> "$GNUPLOT"
echo "  set y2tics 20 nomirror tc lt 2" >> "$GNUPLOT"


TMP='temp.tmp'
cat $TMP >> $GNUPLOT
rm $TMP

echo "  replot \"timereports.gnuplot\" using 1:2 title \"Reported hours\" axis x2y2 with lines ls 10 " >> $GNUPLOT 

echo "timereports plotted" 
   # only pause if displayed in window
   if [ "$FORMAT" = "x11" ]
   then
      echo "pause -1" >> "$GNUPLOT"
   else
       echo "$TERM" >> "$GNUPLOT"
       echo "set size 1,1" >> $GNUPLOT
       echo "set term post portrait color \"Times-Roman\" 12" >> $GNUPLOT
       echo "$OUT" >> "$GNUPLOT"
       echo "replot" >> "$GNUPLOT"
   fi
   # echo "call gnuplot"
    echo "PDF File= $PDF_FILE"
   #  `/usr/local/bin/gnuplot $GNUPLOT`
    /usr/local/bin/gnuplot $GNUPLOT

   # wait a bit to finisg creating the pdf file before moving that 
   sleep 1
   mv temp_output.pdf $PDF_FILE
  #    return $TRUE
}
# script execution
F=${1:-monthly} ;
OUTPUT=${2:-"result.p"} ;
PDF_FILE=${3:-"result.pdf"} ;
# F="weekly" ;  
# F="monthly" ;   
LINES="no"
TITLE="no"
FORMAT="x11"
WIDTH="800"
HEIGHT="600"
GNUPLOT=""
GNUPLOT_OPTIONS=""

ls -1 *_${F}.csv | \
while read i ; do 
  A=`basename "$i" .csv`.gnuplot
  ~/external/gnuplot/csv2gnuplot.sh -i "$i"  -o "$A"
  # echo "$A"
done
# call the function to cretae the script for us
create_gnuplot_script 5 6 "Monthly statistics for all Riak tickets sent towards ESL" "monthly_created.pdf" "pdf"  
create_gnuplot_script 5 7 "Monthly statistics for all Riak tickets solved by ESL" "monthly_solved.pdf" "pdf"
create_gnuplot_script 5 8 "Monthly statistics for all Riak tickets commented by ESL and the customer" "monthly_commented.pdf" "pdf" "number of comments" "x1y1"


