import std/[sequtils, stats, strformat, times]
import nimpy

type
  Feedback = object
    tasteScore: float
    convenienceScore: float
    packagingScore: float
    date: DateTime

  AnalysisResult = object
    meanScores: tuple[taste, convenience, packaging: float]
    stdDevs: tuple[taste, convenience, packaging: float]
    trends: seq[tuple[date: DateTime, avgScore: float]]
    satisfactionLevel: string

proc initPlot() =
  discard pyImport("matplotlib.pyplot")

proc analyzeFeedback(feedback: seq[Feedback]): AnalysisResult =
  let
    tasteScores = feedback.mapIt(it.tasteScore)
    convenienceScores = feedback.mapIt(it.convenienceScore)
    packagingScores = feedback.mapIt(it.packagingScore)
    
  result.meanScores = (
    taste: mean(tasteScores),
    convenience: mean(convenienceScores),
    packaging: mean(packagingScores)
  )
  
  result.stdDevs = (
    taste: standardDeviation(tasteScores),
    convenience: standardDeviation(convenienceScores),
    packaging: standardDeviation(packagingScores)
  )
  
  let overallMean = (result.meanScores.taste + 
                     result.meanScores.convenience + 
                     result.meanScores.packaging) / 3.0
                     
  result.satisfactionLevel = case overallMean
    of 0.0..2.0: "Low"
    of 2.0..3.5: "Medium"
    else: "High"

  # Calculate trends over time
  var dateGroups: seq[tuple[date: DateTime, scores: seq[float]]]
  for item in feedback:
    let idx = dateGroups.find(proc(x: tuple[date: DateTime, 
                                           scores: seq[float]]): bool = 
      x.date.yearday == item.date.yearday)
    
    let avgScore = (item.tasteScore + item.convenienceScore + 
                    item.packagingScore) / 3.0
    if idx >= 0:
      dateGroups[idx].scores.add(avgScore)
    else:
      dateGroups.add((date: item.date, scores: @[avgScore]))

  result.trends = dateGroups.mapIt((
    date: it.date, 
    avgScore: mean(it.scores)
  ))

proc visualizeTrends(analysis: AnalysisResult) =
  let plt = pyImport("matplotlib.pyplot")
  
  let
    dates = analysis.trends.mapIt(it.date.toTime.toUnix.float)
    scores = analysis.trends.mapIt(it.avgScore)

  plt.figure(figsize = (10, 6))
  plt.plot(dates, scores, "b-", label = "Average Score")
  plt.title("Customer Satisfaction Trend")
  plt.xlabel("Date")
  plt.ylabel("Score")
  plt.grid(true)
  plt.legend()
  plt.savefig("satisfaction_trend.png")
  plt.close()

proc visualizeMetrics(analysis: AnalysisResult) =
  let plt = pyImport("matplotlib.pyplot")
  
  let
    categories = @["Taste", "Convenience", "packaging"]
    means = @[analysis.meanScores.taste, 
              analysis.meanScores.convenience,
              analysis.meanScores.packaging]
    errors = @[analysis.stdDevs.taste,
              analysis.stdDevs.convenience,
              analysis.stdDevs.packaging]

  plt.figure(figsize = (8, 6))
  plt.bar(categories, means, yerr = errors, capsize = 5)
  plt.title("Customer Feedback Metrics")
  plt.ylabel("Score")
  plt.ylim(0, 5)
  plt.savefig("feedback_metrics.png")
  plt.close()

proc generateReport*(feedback: seq[Feedback]): string =
  let analysis = analyzeFeedback(feedback)
  
  initPlot()
  visualizeTrends(analysis)
  visualizeMetrics(analysis)
  
  result = fmt"""Customer Feedback Analysis Report
  
Overall Satisfaction: {analysis.satisfactionLevel}

Average Scores:
- Taste: {analysis.meanScores.taste:.2f} (±{analysis.stdDevs.taste:.2f})
- Convenience: {analysis.meanScores.convenience:.2f} (±{analysis.stdDevs.convenience:.2f})
- Packaging: {analysis.meanScores.packaging:.2f} (±{analysis.stdDevs.packaging:.2f})

Visualizations generated:
- satisfaction_trend.png: Shows score trends over time
- feedback_metrics.png: Shows average scores with standard deviations

Sample size: {feedback.len} responses"""

import sequtils, strutils, tables, math, nimbleplot

type
  Feedback = object
    taste: float
    convenience: float
    packaging: float

var
  feedbackData: seq[Feedback]

proc loadData(filepath: string) =
  for line in lines(filepath):
    let parts = line.split(',')
    let feedback = Feedback(
      taste: parseFloat(parts[0]),
      convenience: parseFloat(parts[1]),
      packaging: parseFloat(parts[2])
    )
    feedbackData.add(feedback)

proc calculateMetrics(data: seq[Feedback]): Table[string, float] =
  var metrics = initTable[string, float]()
  let tasteScores = data.mapIt(it.taste)
  let convenienceScores = data.mapIt(it.convenience)
  let packagingScores = data.mapIt(it.packaging)

  metrics["taste_mean"] = mean(tasteScores)
  metrics["convenience_mean"] = mean(convenienceScores)
  metrics["packaging_mean"] = mean(packagingScores)
  metrics["taste_stddev"] = stddev(tasteScores)
  metrics["convenience_stddev"] = stddev(convenienceScores)
  metrics["packaging_stddev"] = stddev(packagingScores)

  return metrics

proc visualizeData(data: seq[Feedback]) =
  var plot = newPlot()
  plot.title = "Customer Feedback Analysis"
  plot.xlabel = "Feedback Index"
  plot.ylabel = "Scores"

  let tasteScores = data.mapIt(it.taste)
  let convenienceScores = data.mapIt(it.convenience)
  let packagingScores = data.mapIt(it.packaging)

  plot.addLine(tasteScores, "Taste Scores")
  plot.addLine(convenienceScores, "Convenience Scores")
  plot.addLine(packagingScores, "Packaging Scores")

  plot.save("feedback_analysis.png")

proc main() =
  loadData("path/to/feedback.csv")
  let metrics = calculateMetrics(feedbackData)
  echo "Metrics: ", metrics
  visualizeData(feedbackData)

main()