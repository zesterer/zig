const std = @import("../../index.zig");
const linux = std.os.linux;
const assert = std.debug.assert;

test "timer" {
    const epoll_fd = linux.epoll_create();
    var err = linux.getErrno(epoll_fd);
    assert(err == 0);

    const timer_fd = linux.timerfd_create(linux.CLOCK_MONOTONIC, 0);
    assert(linux.getErrno(timer_fd) == 0);

    const time_interval = linux.timespec {
        .tv_sec = 0,
        .tv_nsec = 2000000
    };

    const new_time = linux.itimerspec {
        .it_interval = time_interval,
        .it_value = time_interval
    };

    err = linux.timerfd_settime(i32(timer_fd), 0, &new_time, null);
    assert(err == 0);

    var event = linux.epoll_event {
        .events = linux.EPOLLIN | linux.EPOLLOUT | linux.EPOLLET,
        .data = 0
    };

    err = linux.epoll_ctl(i32(epoll_fd), linux.EPOLL_CTL_ADD, i32(timer_fd), &event);
    assert(err == 0);

    const events_one: linux.epoll_event = undefined;
    var events = []linux.epoll_event{events_one} ** 8;

    err = linux.epoll_wait(i32(epoll_fd), &events[0], 8, -1);
}
