-ifndef(_mondemand_server_internal_included).
-define(_mondemand_server_internal_included, yup).

-record (mds_dispatch, { annotation_msg = [],
                         log_msg = [],
                         perf_msg = [],
                         stats_msg = [],
                         trace_msg = []
                       }).

-endif.
