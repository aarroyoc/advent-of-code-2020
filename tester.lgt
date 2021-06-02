:- initialization((
    set_logtalk_flag(report, warnings),
    logtalk_load(lgtunit(loader)),
    logtalk_load('test/day1', [hook(lgtunit)]),
    logtalk_load('test/day2', [hook(lgtunit)]),
    day1::run,
    day2::run
)).