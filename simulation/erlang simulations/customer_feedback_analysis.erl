-module(customer_feedback_analysis).
-export([init/0, add_feedback/4, categorize_feedback/0, generate_report/0]).

-record(feedback, {
    id,
    taste_score,
    convenience_score,
    packaging_score,
    timestamp
}).

init() ->
    ets:new(feedback_table, [named_table, set, public]),
    ets:new(categorized_feedback, [named_table, bag, public]),
    ok.

add_feedback(Id, TasteScore, ConvenienceScore, PackagingScore) ->
    Feedback = #feedback{
        id = Id,
        taste_score = TasteScore,
        convenience_score = ConvenienceScore,
        packaging_score = PackagingScore,
        timestamp = erlang:system_time(second)
    },
    ets:insert(feedback_table, {Id, Feedback}).

categorize_feedback() ->
    ets:delete_all_objects(categorized_feedback),
    CategoryFun = fun({_, Feedback}) ->
        AvgScore = (Feedback#feedback.taste_score + 
                    Feedback#feedback.convenience_score + 
                    Feedback#feedback.packaging_score) / 3,
        Category = case AvgScore of
            Score when Score >= 4.0 -> excellent;
            Score when Score >= 3.0 -> good;
            Score when Score >= 2.0 -> fair;
            _ -> poor
        end,
        ets:insert(categorized_feedback, {Category, Feedback#feedback.id})
    end,
    ets:foldl(CategoryFun, [], feedback_table).

calculate_metrics() ->
    MetricsFun = fun({_, Feedback}, {TasteSum, ConvSum, PackSum, Count}) ->
        {
            TasteSum + Feedback#feedback.taste_score,
            ConvSum + Feedback#feedback.convenience_score,
            PackSum + Feedback#feedback.packaging_score,
            Count + 1
        }
    end,
    {TotalTaste, TotalConv, TotalPack, TotalCount} = 
        ets:foldl(MetricsFun, {0, 0, 0, 0}, feedback_table),
    
    #{
        taste_avg => TotalTaste / TotalCount,
        convenience_avg => TotalConv / TotalCount,
        packaging_avg => TotalPack / TotalCount,
        total_responses => TotalCount
    }.

generate_report() ->
    categorize_feedback(),
    Metrics = calculate_metrics(),
    Categories = lists:map(
        fun(Category) ->
            Count = length(ets:lookup(categorized_feedback, Category)),
            {Category, Count}
        end,
        [excellent, good, fair, poor]
    ),
    
    #{
        metrics => Metrics,
        categories => maps:from_list(Categories),
        timestamp => erlang:system_time(second)
    }.
customer_feedback_analysis:init(),
customer_feedback_analysis:add_feedback(1, 4.5, 3.8, 4.0),
customer_feedback_analysis:add_feedback(2, 3.0, 2.5, 3.5),
Report = customer_feedback_analysis:generate_report(),
io:format("~p~n", [Report]).