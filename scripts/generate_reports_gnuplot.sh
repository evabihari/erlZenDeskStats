#!/bin/bash
# parameters to the script:
# Parameter1: report type: graph/histogram (default is histogram)
# Parameter2: frequency; default is: monthly
# Parameter3: generated gnuplot script name; default is: result.p

export OUTTXT="/tmp/out_$$.txt"

function convert()
{
# function declaration
    # Parameter1 is the name of the time_reports file
    INPUT=${1:-"timereports.gnuplot"} ;
    echo $INPUT
    sed -e 's/^Subtotal [0-9]*\.\([0-9]*\)\.\([0-9]*\)-[0-9.]*[[:space:]]\([0-9.]*\)$/\2\/\1 \3/;/^Date.*$/d;/^Grand Total.*$/d' $INPUT > $OUTTXT
}

function gnu_script_init_part()
{
    # Parameter1: filename
    # Parameter2: Title
   FILE=$1
   echo "# gnuplot script for '$FILE'" > "$FILE"
   echo "set terminal postscript" >> "$FILE"
   echo "set key left invert" >> "$FILE"
   echo "set grid y" >> "$FILE"
   echo "  set yrange [0 :*]" >> "$FILE"
   echo "  set ylabel \"$YLABEL\" tc lt 1" >> "$FILE"
   echo "  set ytics nomirror" >> "$FILE"
   echo "  set xtics nomirror rotate by -45 scale 0 font \",10\" " >> "$FILE"
   echo "  set key noinvert box" >> "$FILE"
   echo "  set title \"$2\" " >> "$FILE"
   echo "  rgb(r,g,b)=int(255*r)*65536+int(255*g)*256+int(255*b)" >>  "$FILE"
   echo "  do for [i=1:31] { " >> "$FILE"
   echo "      myrand=rand(int(rand(0)*i*100)+i*100) " >> "$FILE"
   echo "      set style line i linecolor rgb rgb(rand(0),rand(0),rand(0)) " >> "$FILE"
   echo "  } " >> "$FILE"
}

function gnu_script_histogram_part()
{
    # Parameter1: type: monthly|weekly
    # Parameter2: filename
    # Paramater3: xtic
    # Parameter4: column
    Type=$1
    FILE=$2
    echo "  set style data histograms" >> "$FILE"
    echo "  set style histogram rowstacked" >> "$FILE"
    echo "  set style fill solid border -1" >> "$FILE"
    echo "  set boxwidth 0.75" >> "$FILE"    
    i=0
    ls -1 *_${F}.gnuplot | \
	while read filename ; do
	    let i++
	    TMP='temp.tmp'
	    title=`basename "$filename" .gnuplot`
	    legend=`echo ${title%_monthly}`
	    SUM_title=`echo "SUM_"$F`
	    # legend=`echo $title | sed -e 's/_monthly$//'`
	    # ($3>10 ? $2 : 1/0)
	    if [ $title == $SUM_title ]
	    then
		echo "SUM"
	    else
		if [ $i == 1 ]
		then
		    echo "plot \"$filename\" using $4:xtic($3) title \"$legend\" noenhanced ls 1 " >> $FILE
		else	   
 		    echo "replot \"$filename\" using $4:xtic($3) title \"$legend\" noenhanced ls $i " >> $FILE
		fi
	    fi
	    echo >> "$FILE"
	done
}

function gnu_script_graph_part()
{
    # Parameter1: type (weekly|monthly)
    # Parameter2: filename
    # Parameter3: XValue
    # Paramater4: YValue
    Type=$1
    echo $Type
    FILE=$2
    AXIS="x1y1"
    if [ $Type == monthly ]
       then
	   TimeStr="\"%Y/%m\" "
	   X_Range="[\"2013/10\":*]"
    else
	TimeStr="\"%Y/%m/%d\" "
	X_Range="[\"2014/10/01\":*]"
    fi
    echo "  set timefmt $TimeStr " >> "$FILE"
    echo "  set xdata time " >> "$FILE"
    echo "  set format x $TimeStr " >> "$FILE"
    echo "  set xrange $X_Range " >> "$FILE"   
    echo "  set yrange [0 :*]" >> "$FILE"
    # the plots
    i=0
    ls -1 *_${F}.gnuplot | \
	while read filename ; do
	    let i++
	    TMP='temp.tmp'
	    title=`basename "$filename" .gnuplot`
	    SUM_title=`echo "SUM_"$F`
	    if [ $title == $SUM_title ]
	    then
		echo "SUM"
	    else
		if [ $i == 1 ]
		then
		    echo "plot \"$filename\" using $3:$4 title \"$title\" noenhanced axis $AXIS with lines ls 1" >> $FILE
		else	   
 		    echo "replot \"$filename\" using $3:$4 title \"$title\" noenhanced axis $AXIS with lines ls $i" >> $FILE
		fi
	    fi
	    echo >> "$FILE"
	done
}

function gnu_script_timereport_graph()
{
   # Parameter: filename
    FILE=$1
    echo "  set timefmt \"%Y/%m\" " >> "$FILE"
    echo "  set x2data time" >> "$FILE"
    echo "  set format x2 \"%Y/%m\" " >> "$FILE"
    echo "  set x2range [\"2013/10\":*]" >> "$FILE"
    echo "  set y2label 'no of reported hours' tc lt 2" >> "$FILE"
    echo "  set y2tics 20 nomirror tc lt 2" >> "$FILE"
    echo "  set x2tics nomirror rotate by 90 scale 0 font \",5\" " >> "$FILE"   
    echo "  replot \"$OUTTXT\" using 1:2 title \"Reported hours\" axis x2y2 with lines lc black " >> $FILE 
    echo "timereports plotted" 
}

function create_output()
{
    # Parameter1: filename
    # Parameter2: FORMAT
    FILE=$1
    FORMAT=$2
    echo " FORMAT=$FORMAT ."
    if [ "$FORMAT" = "pdf" ]
    then
	TERM="set terminal postscript  eps enhanced color solid lw 2 font 'Helvetica,12'"
	OUT="set output '| /usr/local/bin/ps2pdf - temp_output.pdf'"      
    fi

    # only pause if displayed in window
   if [ "$FORMAT" = "x11" ]
   then
      echo "pause -1" >> "$FILE"
   else
       echo "$TERM" >> "$FILE"
       echo "set size 1,1" >> $FILE
       echo "set term post portrait color \"Times-Roman\" 12" >> $FILE
       echo "$OUT" >> "$FILE"
       echo "replot" >> "$FILE"
   fi
}

function create_gnuplot_script()
{
    # function declaration
    # Parameter1 is the column of the X value for the graphs
    # Parameter2 is the column of the Y value for the graphs
    # Parameter3 os the frequency of the graph
    # Parameter4 is the Title of the graph
    # Parameter5 is the name of the pdf file the result will be stored
    # Parameter6 is format (pdf/X11)
    # Parameter7 is the y label (default = "number of tickets")
    # Parameter8 is the axis to be used (default = x1y1)
    
    XTIC=$1
    COLUMN=$2
    FREQ=${3:-"monthly"}
    TITLE=$4
    PDF_FILE=${5:-"result.pdf"} ;
    FORMAT=${6:-"X11"} ;
    YLABEL=${7:-"number of tickets"}
    AXIS=${8:-"x1y1"} ;
    TEMP="SUM_monthly.gnuplot"
    # GNUPLOT="$OUTPUT"
    OUT_FILE=`echo "$FREQ_$COLUMN.p"`
    GNUPLOT="$OUT_FILE"
    # gnuplot script creation
    COUNT=`head -n1 "$TEMP" | wc -w | sed s/" "*//g`
    # build output/format statement

    # init
    gnu_script_init_part $GNUPLOT

    # the plots
    if [ $TYPE = "histogram" ]
       then
	   gnu_script_histogram_part $FREQ $GNUPLOT $XTIC $COLUMN
    else
	    gnu_script_graph_part $FREQ $GNUPLOT $1 $2
    fi

    if [ $FREQ == "monthly" ]
       then
	   # timereport graph
	   gnu_script_timereport_graph $GNUPLOT
       fi

    # gnuplot output handling part
    create_output $GNUPLOT $FORMAT

   # echo "call gnuplot"
    echo "PDF File= $PDF_FILE"
    /usr/local/bin/gnuplot $GNUPLOT

   # wait a bit to finish creating the pdf file before moving that 
   sleep 1
   mv temp_output.pdf $PDF_FILE
  #    return $TRUE
}
# script execution
TYPE=${1:-histogram}
F=${2:-monthly} ;
OUTPUT=${3:-"result.p"} ;
PDF_FILE=${4:-"result.pdf"} ;
# F="weekly" ;  
# F="monthly" ;   
FORMAT="x11"
WIDTH="800"
HEIGHT="600"
GNUPLOT=""

ls -1 *_${F}.csv | \
while read i ; do 
  A=`basename "$i" .csv`.gnuplot
  ~/external/gnuplot/csv2gnuplot.sh -i "$i"  -o "$A"
  # echo "$A"
done
# call the function to create the script for us

convert "journyx_report.txt"

Title=`echo $F"_created.pdf"`
echo $Title
create_gnuplot_script 5 6 $F "Statistics for all Riak tickets sent towards ESL" $Title "pdf"
Title=`echo $F"_solved.pdf"`
create_gnuplot_script 5 7 $F "Statistics for all Riak tickets solved by ESL" $Title "pdf"
Title=`echo $F"_commented.pdf"`
create_gnuplot_script 5 8 $F "Statistics for all Riak tickets commented by ESL and the customer" $Title "pdf" "number of comments" "x1y1" 

cp $OUTTXT "timereports.gnuplot"
rm $OUTTXT
