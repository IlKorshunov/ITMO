//go:build !change

package tabletest

import (
	"math"
	"testing"
	"time"
)

func TestFun(t *testing.T) {
	cases := []struct {
		input string
		want  time.Duration
		err   bool
	}{
		{"1h 30m", 0, true},
		{"100ns", 100 * time.Nanosecond, false},
		{"200µs", 200 * time.Microsecond, false},
		{"-3h45m", -225 * time.Minute, false},
		{"3h 45m", 0, true},
		{"0010m", 10 * time.Minute, false},
		{"10m  ", 0, true},
		{"  10m", 0, true},
		{"10m\t", 0, true},
		{"\n10m", 0, true},
		{"10m\n", 0, true},
		{"2h+45m", 0, true},
		{"-0ms", 0, false},
		{"-0.000ns", 0, false},
		{"10sm", 0, true},
		{"300ms", 300 * time.Millisecond, false},
		{"+300ms", 300 * time.Millisecond, false},
		{"0ms", 0, false},
		{"ms0", 0, true},
		{"300h", 300 * time.Hour, false},
		{"300m", 300 * time.Minute, false},
		{"300s", 300 * time.Second, false},
		{"300ns", 300 * time.Nanosecond, false},
		{"300us", 300 * time.Microsecond, false},
		{"9223372036854778888885808ns", 0, true},
		{"-2.5h", -150 * time.Minute, false},
		{"1h30m10s", 5410 * time.Second, false},
		{"", 0, true},
		{" ", 0, true},
		{"300", 0, true},
		{"2h30m", 150 * time.Minute, false},
		{".5h", 30 * time.Minute, false},
		{".h", 0, true},
		{"1000000000000000000000h", 0, true},
		{"2.5.6h", 0, true},
		{"300qq", 0, true},
		{"2147483647s", math.MaxInt32 * time.Second, false},
		{"-2147483647s", -math.MaxInt32 * time.Second, false},
		{"1ы", 0, true},
		{"2h-45m", 0, true},
		{"-9223372036854775807ns", -(1<<63 - 1) * time.Nanosecond, false},
		{"9223372036854775808µs", 0, true},
		{"2147483648µs", 2147483648 * time.Microsecond, false},
		{"-2147483649µs", -2147483649 * time.Microsecond, false},
		{"1.2.3s", 0, true},
		{"1m1.0s", 61 * time.Second, false},
		{"2.000000000000000001h", 2 * time.Hour, false},
		{"-2.000000000000000001h", -2 * time.Hour, false},
		{"1m0.000000001s", 60*time.Second + 1*time.Nanosecond, false},
		{"1.0000000001s", 1 * time.Second, false},
		{"10000000000ns", 10 * time.Second, false},
		{"10000000001ns", 10*time.Second + 1*time.Nanosecond, false},
		{"-10000000000ns", -10 * time.Second, false},
		{"-10000000001ns", -10*time.Second - 1*time.Nanosecond, false},
		{"1000000000ns", 1 * time.Second, false},
		{"1000000001ns", 1*time.Second + 1*time.Nanosecond, false},
		{"-1000000000ns", -1 * time.Second, false},
		{"-1000000001ns", -1*time.Second - 1*time.Nanosecond, false},
		{"9223372036854775809", 0, true},
		{".s", 0, true},
		// cow
	}
	for _, tc := range cases {
		time, err := ParseDuration(tc.input)
		if tc.err == false && err != nil {
			t.Errorf("Unexpected mistake : %s", err)
		} else if tc.err == true && err == nil {
			t.Errorf("There is not enough such error : %t", tc.err)
		} else if tc.err == false && tc.want != time {
			t.Errorf("%v, want %v", time, tc.want)
		}
	}
}
