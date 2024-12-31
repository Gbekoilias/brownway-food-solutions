require "csv"
require "stats"

module CustomerFeedback
  class Analysis
    @feedback_data : Array(NamedTuple(
      taste: Float64,
      convenience: Float64, 
      packaging: Float64
    ))

    def initialize
      @feedback_data = [] of NamedTuple(
        taste: Float64,
        convenience: Float64,
        packaging: Float64
      )
    end

    def load_data(filepath : String)
      CSV.each_row(File.read(filepath)) do |row|
        @feedback_data << {
          taste: row[0].to_f,
          convenience: row[1].to_f,
          packaging: row[2].to_f
        }
      end
    end

    def calculate_metrics
      taste_scores = @feedback_data.map(&.[:taste])
      convenience_scores = @feedback_data.map(&.[:convenience])
      packaging_scores = @feedback_data.map(&.[:packaging])

      {
        taste: {
          mean: Stats.mean(taste_scores),
          std_dev: Stats.standard_deviation(taste_scores),
          median: Stats.median(taste_scores)
        },
        convenience: {
          mean: Stats.mean(convenience_scores),
          std_dev: Stats.standard_deviation(convenience_scores),
          median: Stats.median(convenience_scores)
        },
        packaging: {
          mean: Stats.mean(packaging_scores),
          std_dev: Stats.standard_deviation(packaging_scores),
          median: Stats.median(packaging_scores)
        }
      }
    end

    def identify_concerns(threshold : Float64 = 3.5)
      metrics = calculate_metrics
      concerns = [] of String

      metrics.each do |category, stats|
        concerns << category.to_s if stats[:mean] < threshold
      end

      concerns
    end

    def generate_report
      metrics = calculate_metrics
      concerns = identify_concerns

      {
        metrics: metrics,
        concerns: concerns,
        sample_size: @feedback_data.size,
        timestamp: Time.utc
      }
    end
  end
end

    def identify_concerns(threshold : Float64 = 3.5)
      metrics = calculate_metrics
      concerns = [] of String

      metrics.each do |category, stats|
        concerns << category.to_s if stats[:mean] < threshold
      end

      concerns
    end

    def display_results
      metrics = calculate_metrics
      concerns = identify_concerns

      puts "Customer Feedback Analysis Results:"
      metrics.each do |category, stats|
        puts "#{category.capitalize}:"
        puts "  Mean: #{stats[:mean]}"
        puts "  Standard Deviation: #{stats[:std_dev]}"
        puts "  Median: #{stats[:median]}"
      end

      if concerns.any?
        puts "\nAreas of concern (mean score below threshold):"
        concerns.each do |concern|
          puts "  - #{concern.capitalize}"
        end
      else
        puts "\nNo areas of concern."
      end
    end
  end
end

# Example usage:
# analysis = CustomerFeedback::Analysis.new
# analysis.load_data("path/to/feedback.csv")
# analysis.display_results