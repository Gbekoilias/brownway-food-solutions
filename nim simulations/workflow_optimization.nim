import std/[asyncdispatch, tables, times, strformat, sequtils]

type
  StageStatus = enum
    Waiting, InProgress, Completed
    
  Stage = ref object
    name: string
    duration: float
    resources: int
    dependencies: seq[string]
    status: StageStatus
    startTime: float
    progress: float
    resourceUtilization: float

  Bottleneck = object
    stageName: string
    utilizationRate: float
    impact: float

  WorkflowSimulation = ref object
    stages: TableRef[string, Stage]
    startTime: float
    totalResources: int
    bottlenecks: seq[Bottleneck]

proc newStage(name: string, duration: float, resources: int, 
              dependencies: seq[string] = @[]): Stage =
  Stage(
    name: name,
    duration: duration,
    resources: resources,
    dependencies: dependencies,
    status: Waiting,
    progress: 0.0,
    resourceUtilization: 0.0
  )

proc newWorkflowSimulation(): WorkflowSimulation =
  result = WorkflowSimulation(
    stages: newTable[string, Stage](),
    totalResources: 0,
    bottlenecks: @[]
  )

proc addStage*(workflow: WorkflowSimulation, name: string, duration: float,
               resources: int, dependencies: seq[string] = @[]) =
  workflow.stages[name] = newStage(name, duration, resources, dependencies)
  workflow.totalResources += resources

proc isDependenciesCompleted(workflow: WorkflowSimulation, stage: Stage): bool =
  for dep in stage.dependencies:
    if workflow.stages[dep].status != Completed:
      return false
  return true

proc calculateStageProgress(stage: Stage): float =
  if stage.status != InProgress:
    return 0.0
  result = (epochTime() - stage.startTime) / (stage.duration * 86400.0) * 100.0
  if result > 100.0:
    result = 100.0

proc identifyBottlenecks(workflow: WorkflowSimulation) =
  workflow.bottlenecks = @[]
  let activeStages = workflow.stages.values.toSeq.filterIt(it.status == InProgress)
  
  for stage in activeStages:
    let utilization = stage.resourceUtilization
    if utilization > 0.8:  # 80% threshold
      workflow.bottlenecks.add(Bottleneck(
        stageName: stage.name,
        utilizationRate: utilization,
        impact: (stage.duration * stage.resources.float) / 
                workflow.totalResources.float
      ))

proc updateResourceUtilization(workflow: WorkflowSimulation) =
  var totalActiveResources = 0
  for stage in workflow.stages.values:
    if stage.status == InProgress:
      totalActiveResources += stage.resources
      stage.resourceUtilization = stage.resources.float / 
                                 workflow.totalResources.float

async proc simulateStage(workflow: WorkflowSimulation, stage: Stage) {.async.} =
  stage.status = InProgress
  stage.startTime = epochTime()
  
  while stage.progress < 100.0:
    stage.progress = calculateStageProgress(stage)
    await sleepAsync(1000)  # Update every second
    
  stage.status = Completed

proc optimizeWorkflow*(workflow: WorkflowSimulation): Future[void] {.async.} =
  workflow.startTime = epochTime()
  var futures: seq[Future[void]] = @[]
  
  while true:
    for stage in workflow.stages.values:
      if stage.status == Waiting and isDependenciesCompleted(workflow, stage):
        futures.add(simulateStage(workflow, stage))
    
    updateResourceUtilization(workflow)
    identifyBottlenecks(workflow)
    
    if workflow.stages.values.toSeq.allIt(it.status == Completed):
      break
      
    await sleepAsync(100)
  
  await all(futures)

proc generateOptimizationReport*(workflow: WorkflowSimulation): string =
  var totalDuration = epochTime() - workflow.startTime
  var completedStages = workflow.stages.values.toSeq.filterIt(it.status == Completed)
  
  result = fmt"""Workflow Optimization Report
  
Total Duration: {totalDuration/86400.0:.2f} days
Completed Stages: {completedStages.len}/{workflow.stages.len}

Identified Bottlenecks:"""

  for bottleneck in workflow.bottlenecks:
    result &= fmt"""
- {bottleneck.stageName}:
  Utilization: {bottleneck.utilizationRate*100:.1f}%
  Impact: {bottleneck.impact*100:.1f}%"""

  if workflow.bottlenecks.len == 0:
    result &= "\nNo significant bottlenecks detected."


import asyncdispatch, sequtils, strutils, tables, times

type
  Stage = object
    name: string
    duration: int
    resources: int

var
  stages: seq[Stage]

proc addStage(name: string, duration: int, resources: int) =
  let stage = Stage(name: name, duration: duration, resources: resources)
  stages.add(stage)

proc simulateWorkflow(): Table[string, int] {.async.} =
  var results = initTable[string, int]()
  for stage in stages:
    await sleepAsync(stage.duration)
    results[stage.name] = stage.duration
  return results

proc calculateEfficiencyMetrics(): Table[string, float] =
  var metrics = initTable[string, float]()
  let totalDuration = stages.mapIt(it.duration).sum()
  let totalResources = stages.mapIt(it.resources).sum()
  metrics["total_duration"] = float(totalDuration)
  metrics["total_resources"] = float(totalResources)
  metrics["average_duration"] = float(totalDuration) / stages.len.float
  metrics["average_resources"] = float(totalResources) / stages.len.float
  return metrics

proc identifyInefficiencies(metrics: Table[string, float]): seq[string] =
  var inefficiencies = @[]
  if metrics["total_duration"] > 100.0:
    inefficiencies.add("Consider reducing the duration of some stages")
  if metrics["total_resources"] > 50.0:
    inefficiencies.add("Optimize resource allocation")
  return inefficiencies

proc generateReport(): Table[string, JsonNode] {.async.} =
  let metrics = calculateEfficiencyMetrics()
  let inefficiencies = identifyInefficiencies(metrics)
  let results = await simulateWorkflow()
  var report = initTable[string, JsonNode]()
  report["metrics"] = %*metrics
  report["inefficiencies"] = %*inefficiencies
  report["results"] = %*results
  report["timestamp"] = %$(now())
  return report

proc main() {.async.} =
  addStage("Design", 10, 5)
  addStage("Development", 20, 10)
  addStage("Testing", 5, 3)
  let report = await generateReport()
  echo "Report: ", report

waitFor main()