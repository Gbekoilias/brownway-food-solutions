# workflow_optimization.cr

module ProductDevelopment
      class WorkflowOptimization
        class Stage
          property name : String
          property duration : Float64
          property resources : Int32
          property dependencies : Array(String)
    
          def initialize(@name, @duration, @resources, @dependencies = [] of String)
          end
        end
    
        property stages : Hash(String, Stage)
        
        def initialize
          @stages = {} of String => Stage
        end
    
        def add_stage(name : String, duration : Float64, resources : Int32, dependencies = [] of String)
          @stages[name] = Stage.new(name, duration, resources, dependencies)
        end
    
        def calculate_critical_path
          earliest_start = {} of String => Float64
          earliest_finish = {} of String => Float64
    
          # Forward pass
          @stages.each do |name, stage|
            if stage.dependencies.empty?
              earliest_start[name] = 0.0
            else
              earliest_start[name] = stage.dependencies.map { |dep|
                earliest_finish[dep]
              }.max
            end
            earliest_finish[name] = earliest_start[name] + stage.duration
          end
    
          {
            "total_duration" => earliest_finish.values.max,
            "stage_timings" => earliest_start.map { |name, start|
              {name => {
                "start" => start,
                "finish" => earliest_finish[name]
              }}
            }.reduce({} of String => Hash(String, Float64)) { |acc, h| acc.merge(h) }
          }
        end
    
        def identify_bottlenecks
          critical_path = calculate_critical_path
          bottlenecks = [] of Hash(String, Float64 | Int32)
    
          @stages.each do |name, stage|
            utilization = (stage.duration * stage.resources) / critical_path["total_duration"]
            if utilization > 0.8  # 80% threshold for bottleneck identification
              bottlenecks << {
                "stage" => name,
                "utilization" => utilization,
                "resources" => stage.resources
              }
            end
          end
    
          bottlenecks
        end
    
        def optimize_workflow
          bottlenecks = identify_bottlenecks
          recommendations = [] of String
    
          bottlenecks.each do |bottleneck|
            stage_name = bottleneck["stage"]
            recommendations << "Increase resources for #{stage_name} stage"
            recommendations << "Consider parallel processing in #{stage_name} stage"
          end
    
          {
            "bottlenecks" => bottlenecks,
            "recommendations" => recommendations,
            "current_duration" => calculate_critical_path["total_duration"]
          }
        end
      end
    end
workflow = ProductDevelopment::WorkflowOptimization.new
workflow.add_stage("Design", 10.0, 5)
workflow.add_stage("Development", 20.0, 10, ["Design"])
workflow.add_stage("Testing", 5.0, 3, ["Development"])

optimization_results = workflow.optimize_workflow
puts optimization_results    