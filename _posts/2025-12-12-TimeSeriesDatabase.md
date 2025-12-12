---
layout: post
title: Time Series Database fundamentals
published: false
---



# The ADTs

{% highlight OCaml %}

open Bin_prot.Std
open Bigarray

module  Datapoint_vector = CCVector

type data_point= {
	tstamp  :    float;
	value :   int64;
}
[@@deriving bin_io]

module Compressed_data_vector =  CCVector

type timeseries_block = {

    start_time : float;

    points: data_point Datapoint_vector.vector;

    compressed_data: ((int,int8_unsigned_elt, c_layout) Array1.t) Compressed_data_vector.vector;

    compressed_size: int64;
}

module  Timeseries_block_vector = CCVector

type time_series = {

    key : string;

    mutable active_block: timeseries_block ;

    closed_blocks:timeseries_block Timeseries_block_vector.vector;

}

module  Timeseries_vector = CCVector

module  Timeseriesmap = struct
  type t = string
  let compare s s1 =
     String.compare s s1
end

module  Indice_map = CCMap.Make(Timeseriesmap)

module  Indice_vector = CCVector

type  time_series_map = {

    series_vector: time_series option Timeseries_vector.vector;

	mutable index_map : int64  Indice_map.t;

    free_indices: int64 Indice_vector.vector;
}


module type TSDBOperator = sig
  val time_series : unit -> timeseries_block
end

{% endhighlight  %}

# My current code

{% highlight OCaml %}

open Timedesc
open Types

module TimeSeries = struct

let time_since (time : float) =

  let time_as_float =
  (match (Timedesc.of_timestamp_float_s time) with
    | Some t ->
      let time_as_float = Timedesc.Span.sub (Timedesc.Timestamp.now())  (Timedesc.to_timestamp_single t) in
      (match (Timedesc.Time.of_span time_as_float) with
      | Some t ->
         (Time.hour  t, Time.ns t)
      | None -> failwith "Duration error" )
    | None -> failwith "Duration error" )
  in time_as_float


let overlaps start (en_d:float)=
  (match (Timedesc.of_timestamp_float_s (Float.add start  7200.)),
         (Timedesc.of_timestamp_float_s start) ,
         (Timedesc.of_timestamp_float_s en_d) with
    | Some t, Some t1,Some  t2 ->
         (Timedesc.Timestamp.lt (Timedesc.to_timestamp_single t2) (Timedesc.to_timestamp_single t))
          || (Timedesc.Timestamp.gt (Timedesc.to_timestamp_single t)
                                    (Timedesc.to_timestamp_single t1))
    | _,_,  _ ->  failwith "Duration error" )

let time_series() =

        let  start = Float.mul 1000.  (Unix.gettimeofday()) in
        {
            start_time = start;

            points = Datapoint_vector.create();

            compressed_data = Compressed_data_vector.create();

            compressed_size  = 0L;
        }

let new_time_series k =

        {
            key = k;
            active_block  = time_series();
            closed_blocks = Timeseries_block_vector.create();
         }

let get_points timeseries start e_nd =

   let overlaps tstamp  =
    (match (Timedesc.of_timestamp_float_s tstamp),
           (Timedesc.of_timestamp_float_s start),
           (Timedesc.of_timestamp_float_s e_nd) with
    | Some t, Some t1,Some  t2 ->
        (Timedesc.Timestamp.ge (Timedesc.to_timestamp_single t) (Timedesc.to_timestamp_single t1))
        &&
        (Timedesc.Timestamp.le (Timedesc.to_timestamp_single t) (Timedesc.to_timestamp_single t2))
    | _,_, _ ->  failwith "Duration error" )
    in
    CCVector.filter ( fun dp -> overlaps dp.tstamp ) timeseries

let  insert_data_point time_s timestamp value =
      let dp = { tstamp = timestamp; value = value } in
      match (time_since timestamp) with
      (* I should use the time API to check this. *)
      | (h, ns) -> if h > 2 || (h = 2 && ns > 0 ) then( (*TODO  I check nanoseconds only!*)
                     let () = CCVector.push time_s.closed_blocks time_s.active_block in time_s.active_block <- time_series();
                         CCVector.push time_s.active_block.points dp
                     )
                     else(
                         CCVector.push time_s.active_block.points dp
                     )


let query start en_d blocks =
        let  results = CCVector.create() in

        CCVector.iter(fun block ->
            if overlaps block.start_time en_d then
                CCVector.push results (get_points block.points start en_d)
            else ()
        )blocks.closed_blocks;
        if overlaps blocks.active_block.start_time  en_d then
            CCVector.push results (get_points blocks.active_block.points
                            start en_d);

        results

let  get ts_map key =
  match( Indice_map.find_opt key ts_map.index_map) with
  | Some index ->
             Indice_vector.get ts_map.series_vector (Int64.to_int index)
  | None -> failwith "Index doesn't exist"

let insert ts ts_map key timestamp value =
  let index =  get ts_map key in
  match ( CCVector.exists (fun ts -> true ) ts_map.series_vector ) with
  | true ->  insert_data_point ts timestamp value
  | false ->
            let  series = new_time_series  key in
            let () = insert_data_point series timestamp value in
            let index = CCVector.pop ts_map.free_indices in
            match index with
            |Some i ->
                CCVector.set ts_map.series_vector (Int64.to_int i) (Some series)
            |None ->
                let() = CCVector.push ts_map.series_vector (Some series) in
                ts_map.index_map <- Indice_map.update key (fun _ -> index) ts_map.index_map


(* Read paper to understand tombstones *)
let delete ts ts_map key =
  match( Indice_map.find_opt key ts_map.index_map ) with
  | Some index ->
            let () = CCVector.set ts_map.series_vector ts None in
            let () = CCVector.push ts_map.free_indices index in
            Indice_map.remove key ts_map.index_map
  | None -> failwith "Key to be delete is not found"


end

module TSDBOp = TimeSeries

{% endhighlight %}
