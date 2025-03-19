//go:build !solution

package main

import (
	"encoding/json"
	"flag"
	"log"
	"net/http"
	"os"
	//"os"
	"sort"
	"strconv"
)

type AthletInfoStruct struct {
	Name          string `json:"athlete"`
	Age           int    `json:"age"`
	Country       string `json:"country"`
	Sport         string `json:"sport"`
	Year          int    `json:"year"`
	MedalsByYears map[int]MedalStruct
	GoldMedal     int `json:"gold"`
	Silver        int `json:"silver"`
	Bronze        int `json:"bronze"`
	All           int `json:"total"`
}

type MedalStruct struct {
	GoldMedal int `json:"gold"`
	Silver    int `json:"silver"`
	Bronze    int `json:"bronze"`
	All       int `json:"total"`
}

type Countries struct {
	Country     string `json:"country"`
	GoldMedal   int    `json:"gold"`
	SilverMedal int    `json:"silver"`
	BronzeMedal int    `json:"bronze"`
	All         int    `json:"total"`
}

type AnsAtleth struct {
	Name          string              `json:"athlete"`
	Country       string              `json:"country"`
	Medals        MedalStruct         `json:"medals"`
	MedalsByYears map[int]MedalStruct `json:"medals_by_year"`
}

var localData string
var athletes []AthletInfoStruct

func findAthlete(w http.ResponseWriter, searchName string, sport string) AnsAtleth {
	var outAthlete AnsAtleth
	flag := false
	outAthlete.MedalsByYears = make(map[int]MedalStruct)
	predicate := func(athlete AthletInfoStruct) bool {
		return athlete.Name == searchName && (sport == "" || athlete.Sport == sport)
	}

	for _, a := range athletes {
		if predicate(a) {
			if !flag {
				flag = true
				outAthlete.Name = a.Name
				outAthlete.Country = a.Country
			}
			outAthlete.Medals.GoldMedal += a.GoldMedal
			outAthlete.Medals.Silver += a.Silver
			outAthlete.Medals.Bronze += a.Bronze
			outAthlete.Medals.All += a.GoldMedal + a.Silver + a.Bronze

			curYear := a.Year

			if _, ok := outAthlete.MedalsByYears[curYear]; !ok {
				outAthlete.MedalsByYears[curYear] = MedalStruct{}
			}

			medalsAtYear := outAthlete.MedalsByYears[curYear]
			medalsAtYear.GoldMedal += a.GoldMedal
			medalsAtYear.Silver += a.Silver
			medalsAtYear.Bronze += a.Bronze
			medalsAtYear.All += a.All

			outAthlete.MedalsByYears[curYear] = medalsAtYear
		}
	}

	if !flag {
		http.Error(w, "There is no such athlete", http.StatusNotFound)
		return AnsAtleth{}
	}

	return outAthlete
}

type AthleteSlice []AnsAtleth

func (as AthleteSlice) Swap(i, j int) {
	as[i], as[j] = as[j], as[i]
}

func (as AthleteSlice) Len() int {
	return len(as)
}

func (as AthleteSlice) Less(i, j int) bool {
	if as[i].Medals.GoldMedal != as[j].Medals.GoldMedal {
		return as[i].Medals.GoldMedal > as[j].Medals.GoldMedal
	}

	if as[i].Medals.Silver != as[j].Medals.Silver {
		return as[i].Medals.Silver > as[j].Medals.Silver
	}

	if as[i].Medals.Bronze != as[j].Medals.Bronze {
		return as[i].Medals.Bronze > as[j].Medals.Bronze
	}

	return as[i].Name < as[j].Name
}

func sortAthlete(w http.ResponseWriter, sport string, max int) []AnsAtleth {
	if max == 0 {
		max = 3
	}
	var allAthleths []AnsAtleth
	flags := make(map[string]bool)
	for _, nowSport := range athletes {
		if nowSport.Sport == sport && !flags[nowSport.Name] {
			flags[nowSport.Name] = true
			localAthlet := findAthlete(w, nowSport.Name, sport)
			allAthleths = append(allAthleths, localAthlet)
		}
	}

	if len(allAthleths) == 0 {
		http.Error(w, "There is no such sport", http.StatusNotFound)
		return []AnsAtleth{}
	}

	sort.Sort(AthleteSlice(allAthleths))

	if max > 0 && len(allAthleths) > max {
		return allAthleths[:max]
	}

	return allAthleths
}

func sortCountries(w http.ResponseWriter, year int, max int) []Countries {
	if max == 0 {
		max = 3
	}

	countryMap := make(map[string]Countries)

	for _, athlete := range athletes {
		if athlete.Year == year {
			if _, ok := countryMap[athlete.Country]; !ok {
				countryMap[athlete.Country] = Countries{
					Country:     athlete.Country,
					GoldMedal:   0,
					SilverMedal: 0,
					BronzeMedal: 0,
					All:         0,
				}
			}
			country := countryMap[athlete.Country]
			country.GoldMedal += athlete.GoldMedal
			country.SilverMedal += athlete.Silver
			country.BronzeMedal += athlete.Bronze
			country.All += athlete.GoldMedal + athlete.Silver + athlete.Bronze
			countryMap[athlete.Country] = country
		}
	}

	var countries []Countries
	for _, country := range countryMap {
		countries = append(countries, country)
	}

	if len(countries) == 0 {
		http.Error(w, "There is no such year", http.StatusNotFound)
		return []Countries{}
	}

	sort.Slice(countries, func(i, j int) bool {
		if countries[i].GoldMedal != countries[j].GoldMedal {
			return countries[i].GoldMedal > countries[j].GoldMedal
		}
		if countries[i].SilverMedal != countries[j].SilverMedal {
			return countries[i].SilverMedal > countries[j].SilverMedal
		}
		if countries[i].BronzeMedal != countries[j].BronzeMedal {
			return countries[i].BronzeMedal > countries[j].BronzeMedal
		}
		return countries[i].Country < countries[j].Country
	})

	if max > 0 && len(countries) > max {
		return countries[:max]
	}

	return countries
}

func handler(w http.ResponseWriter, r *http.Request) {
	query := r.URL.Query()
	content, err := os.ReadFile(localData)
	if err != nil {
		http.Error(w, "Invalid file", http.StatusInternalServerError)
		return
	}
	if err := json.Unmarshal(content, &athletes); err != nil {
		http.Error(w, "Invalid JSON", http.StatusInternalServerError)
		return
	}
	w.Header().Set("Content-Type", "application/json")
	switch {
	case query.Has("name"):
		Athlete(w, query.Get("name"))
	case query.Has("sport"):
		Sport(w, query.Get("sport"), query.Get("limit"))
	case query.Has("year"):
		Year(w, query.Get("year"), query.Get("limit"))
	default:
		http.Error(w, "Invalid request", http.StatusBadRequest)
	}
}

func Athlete(w http.ResponseWriter, name string) {
	if name == "" {
		http.Error(w, "Missing name parameter", http.StatusBadRequest)
		return
	}
	athlete := findAthlete(w, name, "")
	if err := json.NewEncoder(w).Encode(athlete); err != nil {
		http.Error(w, "Some error", http.StatusInternalServerError)
	}
}

func Sport(w http.ResponseWriter, sport, limitParam string) {
	if sport == "" {
		http.Error(w, "Missing sport parameter", http.StatusBadRequest)
		return
	}
	var limit int
	var err error
	if limitParam == "" {
		limit = 3
	} else {
		limit, err = strconv.Atoi(limitParam)
		if err != nil {
			http.Error(w, "Invalid limit", http.StatusBadRequest)
			return
		}
	}
	athletes := sortAthlete(w, sport, limit)
	if err := json.NewEncoder(w).Encode(athletes); err != nil {
		http.Error(w, "some error", http.StatusInternalServerError)
	}
}

func Year(w http.ResponseWriter, yearParam, limitParam string) {
	year, err := strconv.Atoi(yearParam)
	if err != nil {
		http.Error(w, "Invalid year parameter", http.StatusBadRequest)
		return
	}
	var limit int
	if limitParam == "" {
		limit = 3
	} else {
		limit, err = strconv.Atoi(limitParam)
		if err != nil {
			http.Error(w, "Invalid limit", http.StatusBadRequest)
			return
		}
	}

	countries := sortCountries(w, year, limit)
	if err := json.NewEncoder(w).Encode(countries); err != nil {
		http.Error(w, "Some error", http.StatusInternalServerError)
	}
}

func main() {
	port := flag.Int("port", 0000, "Port to listen on")
	data := flag.String("data", "./olympics/testdata/olympicWinners.json", "data string")
	flag.Parse()
	localData = *data
	http.HandleFunc("/", handler)
	log.Fatal(http.ListenAndServe("localhost:"+strconv.Itoa(*port), nil))
}
