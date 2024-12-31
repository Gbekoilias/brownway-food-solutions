defmodule WorkflowSimulation do
  use Phoenix.LiveView

  defmodule Stage do
    defstruct [:name, :duration, :resources, :dependencies, :status, :start_time, :completion]
  end

  def mount(_params, _session, socket) do
    if connected?(socket) do
      Phoenix.PubSub.subscribe(MyApp.PubSub, "workflow_updates")
    end

    {:ok,
     assign(socket,
       stages: initialize_stages(),
       simulation_running: false,
       efficiency_metrics: %{},
       time_spent: %{},
       bottlenecks: []
     )}
  end

  def render(assigns) do
    ~H"""
    <div class="workflow-container">
      <div class="control-panel">
        <button phx-click="start_simulation" disabled={@simulation_running}>
          Start Simulation
        </button>
        <button phx-click="reset_simulation" disabled={@simulation_running}>
          Reset
        </button>
      </div>

      <div class="stages-visualization">
        <%= for {name, stage} <- @stages do %>
          <div class={"stage-card #{stage.status}"}>
            <h3><%= name %></h3>
            <div class="progress-bar">
              <div class="progress" style={"width: #{stage.completion}%"}></div>
            </div>
            <div class="metrics">
              <p>Duration: <%= stage.duration %> days</p>
              <p>Resources: <%= stage.resources %></p>
              <p>Completion: <%= stage.completion %>%</p>
            </div>
          </div>
        <% end %>
      </div>

      <div class="metrics-panel">
        <h2>Efficiency Metrics</h2>
        <%= if @bottlenecks != [] do %>
          <div class="bottlenecks">
            <h3>Bottlenecks Detected</h3>
            <ul>
              <%= for bottleneck <- @bottlenecks do %>
                <li><%= bottleneck.stage %> - Utilization: <%= bottleneck.utilization %>%</li>
              <% end %>
            </ul>
          </div>
        <% end %>
      </div>
    </div>
    """
  end

  def handle_event("start_simulation", _, socket) do
    if !socket.assigns.simulation_running do
      Process.send_after(self(), :update_simulation, 1000)
      {:noreply, assign(socket, simulation_running: true)}
    else
      {:noreply, socket}
    end
  end

  def handle_event("reset_simulation", _, socket) do
    {:noreply, assign(socket,
      stages: initialize_stages(),
      simulation_running: false,
      efficiency_metrics: %{},
      time_spent: %{},
      bottlenecks: []
    )}
  end

  def handle_info(:update_simulation, socket) do
    if socket.assigns.simulation_running do
      {updated_stages, metrics} = update_simulation_state(socket.assigns.stages)
      bottlenecks = identify_bottlenecks(updated_stages)

      if simulation_complete?(updated_stages) do
        {:noreply, assign(socket,
          stages: updated_stages,
          simulation_running: false,
          efficiency_metrics: metrics,
          bottlenecks: bottlenecks
        )}
      else
        Process.send_after(self(), :update_simulation, 1000)
        {:noreply, assign(socket,
          stages: updated_stages,
          efficiency_metrics: metrics,
          bottlenecks: bottlenecks
        )}
      end
    else
      {:noreply, socket}
    end
  end

  defp initialize_stages do
    %{
      concept: %Stage{
        name: "Concept Development",
        duration: 5,
        resources: 2,
        dependencies: [],
        status: "waiting",
        completion: 0
      },
      prototype: %Stage{
        name: "Prototyping",
        duration: 10,
        resources: 4,
        dependencies: [:concept],
        status: "waiting",
        completion: 0
      },
      testing: %Stage{
        name: "Testing",
        duration: 7,
        resources: 3,
        dependencies: [:prototype],
        status: "waiting",
        completion: 0
      },
      refinement: %Stage{
        name: "Refinement",
        duration: 8,
        resources: 3,
        dependencies: [:testing],
        status: "waiting",
        completion: 0
      }
    }
  end

  defp update_simulation_state(stages) do
    stages
    |> update_active_stages()
    |> calculate_metrics()
  end

  defp update_active_stages(stages) do
    stages
    |> Enum.map(fn {name, stage} ->
      case {stage.status, dependencies_met?(stage, stages)} do
        {"waiting", true} ->
          {name, %{stage | status: "active", start_time: System.system_time(:second)}}
        {"active", _} ->
          progress = calculate_progress(stage)
          status = if progress >= 100, do: "completed", else: "active"
          {name, %{stage | completion: progress, status: status}}
        _ ->
          {name, stage}
      end
    end)
    |> Enum.into(%{})
  end

  defp dependencies_met?(stage, stages) do
    Enum.all?(stage.dependencies, fn dep ->
      get_in(stages, [dep, :status]) == "completed"
    end)
  end

  defp calculate_progress(stage) do
    if stage.start_time do
      elapsed = System.system_time(:second) - stage.start_time
      progress = (elapsed / (stage.duration * 86400)) * 100
      min(progress, 100)
    else
      0
    end
  end

  defp calculate_metrics(stages) do
    metrics = %{
      total_duration: calculate_total_duration(stages),
      resource_utilization: calculate_resource_utilization(stages),
      completion_percentage: calculate_completion_percentage(stages)
    }
    {stages, metrics}
  end

  defp identify_bottlenecks(stages) do
    stages
    |> Enum.filter(fn {_, stage} ->
      stage.status == "active" &&
      (stage.resources / stage.duration) > 0.5
    end)
    |> Enum.map(fn {name, stage} ->
      %{
        stage: name,
        utilization: (stage.resources / stage.duration) * 100
      }
    end)
  end

  defp simulation_complete?(stages) do
    Enum.all?(stages, fn {_, stage} -> stage.status == "completed" end)
  end

  defp calculate_total_duration(stages) do
    stages
    |> Enum.filter(fn {_, stage} -> stage.status == "completed" end)
    |> Enum.map(fn {_, stage} -> stage.duration end)
    |> Enum.sum()
  end

  defp calculate_resource_utilization(stages) do
    total_resources = Enum.sum(Enum.map(stages, fn {_, stage} -> stage.resources end))
    active_resources = stages
    |> Enum.filter(fn {_, stage} -> stage.status == "active" end)
    |> Enum.map(fn {_, stage} -> stage.resources end)
    |> Enum.sum()

    (active_resources / total_resources) * 100
  end

  defp calculate_completion_percentage(stages) do
    completed = Enum.count(stages, fn {_, stage} -> stage.status == "completed" end)
    (completed / map_size(stages)) * 100
  end
end
defmodule WorkflowSimulation do
  use Phoenix.LiveView
  alias WorkflowSimulation.Repo
  alias WorkflowSimulation.WorkflowStep

  import Ecto.Query, only: [from: 2]
  require Logger

  defmodule WorkflowStep do
    use Ecto.Schema

    schema "workflow_steps" do
      field :name, :string
      field :duration, :float
      field :resources, :integer
      timestamps()
    end
  end

  def start_link do
    {:ok, _} = Repo.start_link()
  end

  def add_step(name, duration, resources) do
    %WorkflowStep{
      name: name,
      duration: duration,
      resources: resources
    }
    |> Repo.insert!()
  end

  def calculate_efficiency_metrics do
    query = from ws in WorkflowStep,
            select: %{
              total_duration: sum(ws.duration),
              total_resources: sum(ws.resources),
              average_duration: avg(ws.duration),
              average_resources: avg(ws.resources)
            }

    Repo.one(query)
  end

  def visualize_workflow do
    steps = Repo.all(WorkflowStep)
    Phoenix.LiveView.render(WorkflowSimulationWeb.WorkflowView, "index.html", steps: steps)
  end

  def identify_improvements do
    metrics = calculate_efficiency_metrics()

    improvements = if metrics.total_duration > 100 do
      ["Consider reducing the duration of some steps", "Optimize resource allocation"]
    else
      ["Workflow is efficient"]
    end

    %{
      metrics: metrics,
      improvements: improvements
    }
  end

  def generate_report do
    metrics = calculate_efficiency_metrics()
    improvements = identify_improvements()

    %{
      metrics: metrics,
      improvements: improvements,
      timestamp: DateTime.utc_now()
    }
  end
end

# Example usage:
# WorkflowSimulation.start_link()
# WorkflowSimulation.add_step("Design", 10.0, 5)
# WorkflowSimulation.add_step("Development", 20.0, 10)
# WorkflowSimulation.add_step("Testing", 5.0, 3)
# report = WorkflowSimulation.generate_report()
# IO.inspect(report)
