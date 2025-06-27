trait Panel {
  def width() : Int
  def height() : Int
  def getRows() : List[String]
  override def toString() = getRows().foldLeft("")((x, y) => x + "\n" + y)
}

class StringPanel (s : String) extends Panel {
  def width() = s.length
  def height() = 1
  def getRows() = List(s)
  override def toString() = s
}

/* This class defines a panel that contains one string with all characters in it
 * the same as character c. The string is of length l.
 */
class SameCharStringPanel (c : Char, l : Int) extends StringPanel(c.toString.repeat(l)) {
}

class HorizontalPanel(elts : List[Panel]) extends Panel {

  var elements : List[Panel] = List()
  val maxheight = elts.map(e => e.height).max
  for(e <-elts) {
    if(e.height < maxheight) {
      val diff = maxheight - e.height
      val blanks = new RepeatCharPanel(' ', e.width, diff)
      val newe = new VerticalPanel(List(e, blanks))
      elements = elements ++ List(newe)
    }
    else {
      elements = elements ++ List(e)
    }
  }
  def width() = elts.foldLeft(0)((x, y) => x + y.width)

  def height() = maxheight
  def getRows() = {
    var rows = List[String]()
    for(i <- 0 to (height - 1)) {
      var row = elements.foldLeft("")((x, y) => x + y.getRows()(i))
      rows = rows ++ List(row)
    }
    rows
  }

}

/* This class stacks up the constituent panels vertically.
 * As an additional feature, it pads up all these panels in
 * case they are of insufficient width with empty characters.
 * For example, lets say:
 * p1 = ABC  and p2 = ABCDEF
 *      DEF
 * If we create a VerticalPanel with p1 and p2 as constituent panels,
 * the resultant panel will look like this:
 * ABC***
 * DEF***
 * ABCDEF
 *
 * Note that p1 has been padded up with some trailing '*'s to match the width
 * of p2.
 */
class VerticalPanel(elts : List[Panel]) extends Panel {

  var elements : List[Panel] = List()
  val maxwidth = elts.map(e => e.width).max
  for(e <-elts) {
    if(e.width < maxwidth) {
      val diff = maxwidth - e.width
      val blanks = new RepeatCharPanel(' ', diff, e.height)
      val newe = new HorizontalPanel(List(e, blanks))
      elements = elements ++ List(newe)
    }
    else {
      elements = elements ++ List(e)
    }
  }

  def width() = maxwidth
  def height() = elts.foldLeft(0)((x, y) => x + y.height)
  def getRows() = elements.foldLeft(List[String]())((x, y) => x ++ y.getRows)
}

class RepeatHorizontalPanel(p : Panel, n : Int) extends HorizontalPanel(List.fill(n)(p)) {
}

// A vertically aligned panel that repeats the same panel p - n times.
// For example:
// Suppose p =
//
// ABC
// DEF
//
// Then, if we make RepeatVerticalPanel(p, 3), it will generate the following:
//
// ABC
// DEF
// ABC
// DEF
// ABC
// DEF
class RepeatVerticalPanel(p : Panel, n : Int) extends VerticalPanel(List.fill(n)(p)) {
}
/*
 * This class represents a panel which is filled with the same character c in a block
 * that is w wide and h high. This really is a RepeatVerticalPanel made out of a 
 * StringPanel consisting of the same character.
 */
class RepeatCharPanel (c : Char, w : Int, h : Int)
  extends RepeatVerticalPanel(new SameCharStringPanel(c, w), h) {}

abstract class Month(month : Int, year : Int) {
  val sundayHeader    = new StringPanel(Weekday.SUN.toString())
  val mondayHeader    = new StringPanel(Weekday.MON.toString())
  val tuesdayHeader   = new StringPanel(Weekday.TUE.toString())
  val wednesdayHeader = new StringPanel(Weekday.WED.toString())
  val thursdayHeader  = new StringPanel(Weekday.THU.toString())
  val fridayHeader    = new StringPanel(Weekday.FRI.toString())
  val saturdayHeader  = new StringPanel(Weekday.SAT.toString())

  val emptyPanel = new StringPanel(" ")
  val emptyDayPanels = List.fill(Weekday.intOfWeekday(new MyDate(1, month, year).weekday) - 1)(emptyPanel)

  val days = Range.inclusive(1, new MyDate(1, month, year).daysInMonth())
  val dayStrings = days.map(d => d.toString())
  val dayPanels = emptyDayPanels ++ dayStrings.map(strday => new HorizontalPanel(List(new StringPanel(strday), emptyPanel)))

  def panel() : Panel

  override def toString() = panel.toString
}

class Month_weekV(month : Int, year : Int) extends Month(month, year) {
  val allHeaders =
      List(
        sundayHeader, mondayHeader, tuesdayHeader, wednesdayHeader,
        thursdayHeader, fridayHeader, saturdayHeader
      )
  val weekdayPanel = new VerticalPanel(allHeaders)
  val panelsByWeek = dayPanels.grouped(7).toList.map(l => new VerticalPanel(l))

  override def panel() = new HorizontalPanel(weekdayPanel :: emptyPanel :: panelsByWeek)
}

class Month_7_1(month : Int, year : Int) extends Month(month, year) {

  override def panel() = {
    val allHeaders = 
      List(
        sundayHeader, mondayHeader, tuesdayHeader, wednesdayHeader,
        thursdayHeader, fridayHeader, saturdayHeader, 
        sundayHeader, mondayHeader, tuesdayHeader, wednesdayHeader,
        thursdayHeader, fridayHeader, saturdayHeader, 
        sundayHeader, mondayHeader, tuesdayHeader, wednesdayHeader,
        thursdayHeader, fridayHeader, saturdayHeader, 
        sundayHeader, mondayHeader, tuesdayHeader, wednesdayHeader,
        thursdayHeader, fridayHeader, saturdayHeader, 
        sundayHeader, mondayHeader, tuesdayHeader, wednesdayHeader,
        thursdayHeader, fridayHeader, saturdayHeader, 
        sundayHeader, mondayHeader, tuesdayHeader, wednesdayHeader,
        thursdayHeader, fridayHeader, saturdayHeader)
    val vpanels = allHeaders.zip(dayPanels)
                    .map(t =>
                          t match {
                            case (headerPanel : Panel, dayPanel : Panel) => List(headerPanel, dayPanel)
                            case _ => throw new Exception("Something wrong in the panels.")
                          }
                        )
                    .map(l => new VerticalPanel(l))
    val gap = new StringPanel(" ")
    val vpanelsWithGaps = vpanels.map(panel => new HorizontalPanel(List(panel, gap)))
    val allDaysPanel = new HorizontalPanel(vpanelsWithGaps)
    allDaysPanel
  }
}

class Month_1_7(month : Int, year : Int) extends Month(month, year) {

  override def panel() = {
    val allHeaders = 
      List(
        sundayHeader, mondayHeader, tuesdayHeader, wednesdayHeader,
        thursdayHeader, fridayHeader, saturdayHeader, 
        sundayHeader, mondayHeader, tuesdayHeader, wednesdayHeader,
        thursdayHeader, fridayHeader, saturdayHeader, 
        sundayHeader, mondayHeader, tuesdayHeader, wednesdayHeader,
        thursdayHeader, fridayHeader, saturdayHeader, 
        sundayHeader, mondayHeader, tuesdayHeader, wednesdayHeader,
        thursdayHeader, fridayHeader, saturdayHeader, 
        sundayHeader, mondayHeader, tuesdayHeader, wednesdayHeader,
        thursdayHeader, fridayHeader, saturdayHeader, 
        sundayHeader, mondayHeader, tuesdayHeader, wednesdayHeader,
        thursdayHeader, fridayHeader, saturdayHeader)
    
    val gap = new StringPanel(" ")
    val hpanels = allHeaders.zip(dayPanels)
                    .map(t =>
                          t match {
                            case (headerPanel : Panel, dayPanel : Panel) => List(headerPanel, gap, dayPanel)
                            case _ => throw new Exception("Something wrong in the panels.")
                          }
                        )
                    .map(l => new HorizontalPanel(l))
    val allDaysPanel = new VerticalPanel(hpanels)
    allDaysPanel
  }
}

abstract class MonthFactory {
  def january  (year : Int) : Month
  def february (year : Int) : Month
  def march    (year : Int) : Month
  def april    (year : Int) : Month
  def may      (year : Int) : Month
  def june     (year : Int) : Month
  def july     (year : Int) : Month
  def august   (year : Int) : Month
  def september(year : Int) : Month
  def october  (year : Int) : Month
  def november (year : Int) : Month
  def december (year : Int) : Month
}

class Month_7_1_Factory extends MonthFactory {
  def january  (year : Int) = new Month_7_1(1 , year)
  def february (year : Int) = new Month_7_1(2 , year)
  def march    (year : Int) = new Month_7_1(3 , year)
  def april    (year : Int) = new Month_7_1(4 , year)
  def may      (year : Int) = new Month_7_1(5 , year)
  def june     (year : Int) = new Month_7_1(6 , year)
  def july     (year : Int) = new Month_7_1(7 , year)
  def august   (year : Int) = new Month_7_1(8 , year)
  def september(year : Int) = new Month_7_1(9 , year)
  def october  (year : Int) = new Month_7_1(10, year)
  def november (year : Int) = new Month_7_1(11, year)
  def december (year : Int) = new Month_7_1(12, year)
}

class Month_1_7_Factory extends MonthFactory {
  def january  (year : Int) = new Month_1_7(1 , year)
  def february (year : Int) = new Month_1_7(2 , year)
  def march    (year : Int) = new Month_1_7(3 , year)
  def april    (year : Int) = new Month_1_7(4 , year)
  def may      (year : Int) = new Month_1_7(5 , year)
  def june     (year : Int) = new Month_1_7(6 , year)
  def july     (year : Int) = new Month_1_7(7 , year)
  def august   (year : Int) = new Month_1_7(8 , year)
  def september(year : Int) = new Month_1_7(9 , year)
  def october  (year : Int) = new Month_1_7(10, year)
  def november (year : Int) = new Month_1_7(11, year)
  def december (year : Int) = new Month_1_7(12, year)
}

class MonthV_Factory extends MonthFactory {
  def january  (year : Int) = new Month_weekV(1 , year)
  def february (year : Int) = new Month_weekV(2 , year)
  def march    (year : Int) = new Month_weekV(3 , year)
  def april    (year : Int) = new Month_weekV(4 , year)
  def may      (year : Int) = new Month_weekV(5 , year)
  def june     (year : Int) = new Month_weekV(6 , year)
  def july     (year : Int) = new Month_weekV(7 , year)
  def august   (year : Int) = new Month_weekV(8 , year)
  def september(year : Int) = new Month_weekV(9 , year)
  def october  (year : Int) = new Month_weekV(10, year)
  def november (year : Int) = new Month_weekV(11, year)
  def december (year : Int) = new Month_weekV(12, year)
}

abstract class Year(year : Int, monthFactory : MonthFactory) {
  val jan = monthFactory.january(year)
  val feb = monthFactory.february(year)
  val mar = monthFactory.march(year)
  val apr = monthFactory.april(year)
  val may = monthFactory.may(year)
  val jun = monthFactory.june(year)
  val jul = monthFactory.july(year)
  val aug = monthFactory.august(year)
  val sep = monthFactory.september(year)
  val oct = monthFactory.october(year)
  val nov = monthFactory.november(year)
  val dec = monthFactory.december(year)

  def panel() : Panel;
  override def toString() = panel.toString()
}

class Year_1_12(year : Int) extends Year(year, new Month_7_1_Factory) {
  override def panel() = {
    val months = List(jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec)
    val emptyLine = new StringPanel("")
    val monthPanels = months.map(month => new VerticalPanel(List(month.panel, emptyLine)))
    new VerticalPanel(monthPanels)
  }
}

class Year_12_1(year : Int) extends Year(year, new Month_1_7_Factory) {
  override def panel() = {
    val months = List(jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec)
    val emptyLine = new StringPanel(" ")
    val monthPanels = months.map(month => new HorizontalPanel(List(month.panel, emptyLine)))
    new HorizontalPanel(monthPanels)
  }
}

class Year_Month_WeekV_1_12(year : Int) extends Year(year, new MonthV_Factory) {
  override def panel() = {
    val months = List(jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec)
    val emptyLine = new StringPanel("")
    val monthPanels = months.map(month => new VerticalPanel(List(month.panel, emptyLine)))
    new VerticalPanel(monthPanels)
  }
}

class Year_Month_WeekV_4_3(year : Int) extends Year(year, new MonthV_Factory) {
  override def panel() = {
    val months = List(jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec)
    val emptyLine = new StringPanel("")
    val monthPanels = months.map(month => new VerticalPanel(List(month.panel, emptyLine)))
    val monthColumns = monthPanels.grouped(3).toList.map(mcol => new VerticalPanel(mcol))
    new HorizontalPanel(monthColumns.map(mc => new HorizontalPanel(List(mc, new StringPanel("   ")))))
  }
}

object PanelDriver {
  def main(args:Array[String]) = {
/*    // println("Hello panel!")
    val p1 = new StringPanel("Sujit")
    // println(p1)
    val p2 = new SameCharStringPanel('X', 10)
    // println(p1)
    val p3 = new HorizontalPanel(List(p1, p2))
    // println(p3)
    val p4 = new VerticalPanel(List(p1, p2))
    println(p4)
    println(p4.getRows().length)
    val p5 = new HorizontalPanel(List(p1, p4, p3))
    println(p5)
    println("Weekday(03-05-2025) = " + new MyDate(3, 5, 2025).weekday)
    println("Weekday(24-01-2009) = " + new MyDate(24, 1, 2009).weekday)
    println(new Month_7_1(2, 2025))
    println(new Month_7_1(5, 2025))
    println(new Year_1_12(2025))
    println(new Year_12_1(2024))
*/
    println(new Year_Month_WeekV_1_12(2025))
    println(new Year_Month_WeekV_4_3(2025))
  }
}
