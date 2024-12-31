defmodule FeedbackAnalysis do
  use Ecto.Schema
  import Ecto.Query
  alias NimbleCSV.RFC4180, as: CSV

  schema "feedback" do
    field :taste_score, :float
    field :convenience_score, :float
    field :packaging_score, :float
    field :date, :date
    timestamps()
  end

  def import_csv(file_path) do
    file_path
    |> File.stream!()
    |> CSV.parse_stream()
    |> Stream.map(fn [taste, convenience, packaging] ->
      %{
        taste_score: String.to_float(taste),
        convenience_score: String.to_float(convenience),
        packaging_score: String.to_float(packaging),
        date: Date.utc_today()
      }
    end)
    |> Enum.to_list()
    |> insert_feedback()
  end

  defp insert_feedback(feedback_list) do
    feedback_list
    |> Enum.chunk_every(100)
    |> Enum.each(&Repo.insert_all(FeedbackAnalysis, &1))
  end

  def analyze_trends(days \\ 30) do
    date_threshold = Date.add(Date.utc_today(), -days)

    query = from f in FeedbackAnalysis,
      where: f.date >= ^date_threshold,
      group_by: f.date,
      select: %{
        date: f.date,
        avg_taste: avg(f.taste_score),
        avg_convenience: avg(f.convenience_score),
        avg_packaging: avg(f.packaging_score)
      }

    Repo.all(query)
    |> calculate_trend_metrics()
  end

  defp calculate_trend_metrics(data) do
    sorted_data = Enum.sort_by(data, & &1.date)

    %{
      trends: %{
        taste: calculate_metric_trend(sorted_data, :avg_taste),
        convenience: calculate_metric_trend(sorted_data, :avg_convenience),
        packaging: calculate_metric_trend(sorted_data, :avg_packaging)
      },
      latest_averages: List.last(sorted_data),
      sample_size: length(sorted_data)
    }
  end

  defp calculate_metric_trend(data, metric) do
    values = Enum.map(data, &Map.get(&1, metric))
    first = List.first(values)
    last = List.last(values)

    %{
      direction: if(last > first, do: :improving, else: :declining),
      change_rate: (last - first) / length(data),
      volatility: calculate_volatility(values)
    }
  end

  defp calculate_volatility(values) do
    mean = Enum.sum(values) / length(values)

    values
    |> Enum.map(fn x -> :math.pow(x - mean, 2) end)
    |> Enum.sum()
    |> Kernel./(length(values))
    |> :math.sqrt()
  end

  def generate_insights() do
    trends = analyze_trends()

    %{
      trends: trends,
      recommendations: generate_recommendations(trends),
      timestamp: DateTime.utc_now()
    }
  end

  defp generate_recommendations(%{trends: trends}) do
    trends
    |> Enum.filter(fn {_, %{direction: direction}} -> direction == :declining end)
    |> Enum.map(fn {category, _} ->
      "#{category} scores showing decline - review recent changes"
    end)
  end
end

defmodule FeedbackAnalysis do
  alias FeedbackAnalysis.Repo
  alias FeedbackAnalysis.Feedback

  import Ecto.Query, only: [from: 2]
  require Logger

  defmodule Feedback do
    use Ecto.Schema

    schema "feedbacks" do
      field :taste_score, :float
      field :convenience_score, :float
      field :packaging_score, :float
      timestamps()
    end
  end

  def start_link do
    {:ok, _} = Repo.start_link()
  end

  def load_data(filepath) do
    filepath
    |> File.stream!()
    |> NimbleCSV.RFC4180.parse_stream()
    |> Enum.each(fn [taste, convenience, packaging] ->
      %Feedback{
        taste_score: String.to_float(taste),
        convenience_score: String.to_float(convenience),
        packaging_score: String.to_float(packaging)
      }
      |> Repo.insert!()
    end)
  end

  def calculate_metrics do
    query = from f in Feedback,
            select: %{
              taste_avg: avg(f.taste_score),
              convenience_avg: avg(f.convenience_score),
              packaging_avg: avg(f.packaging_score),
              total_responses: count(f.id)
            }

    Repo.one(query)
  end

  def identify_trends do
    query = from f in Feedback,
            select: %{
              taste_trend: fragment("percentile_cont(0.5) WITHIN GROUP (ORDER BY ?)", f.taste_score),
              convenience_trend: fragment("percentile_cont(0.5) WITHIN GROUP (ORDER BY ?)", f.convenience_score),
              packaging_trend: fragment("percentile_cont(0.5) WITHIN GROUP (ORDER BY ?)", f.packaging_score)
            }

    Repo.one(query)
  end

  def generate_report do
    metrics = calculate_metrics()
    trends = identify_trends()

    %{
      metrics: metrics,
      trends: trends,
      timestamp: DateTime.utc_now()
    }
  end
end

# Example usage:
# FeedbackAnalysis.start_link()
# FeedbackAnalysis.load_data("path/to/feedback.csv")
# report = FeedbackAnalysis.generate_report()
# IO.inspect(report)
