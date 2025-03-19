//go:build !solution

package hotelbusiness

import "sort"

type Guest struct {
	CheckInDate      int
	CheckcomputeDate int
}

type Load struct {
	StartDate  int
	GuestCount int
}

func ComputeLoad(guests []Guest) []Load {
	myMap := make(map[int]int)
	var out []Load
	for _, guest := range guests {
		for i := guest.CheckInDate; i <= guest.CheckcomputeDate; i++ {
			if i == guest.CheckcomputeDate {
				myMap[i] += 0
				break
			}
			myMap[i] += 1
		}
	}
	var keys []int
	for key := range myMap {
		keys = append(keys, key)
	}

	sort.Ints(keys)

	var lastCount int
	for _, key := range keys {
		currentCount := myMap[key]
		if currentCount != lastCount {
			out = append(out, Load{
				StartDate:  key,
				GuestCount: currentCount,
			})
			lastCount = currentCount
		}
	}
	return out
}
