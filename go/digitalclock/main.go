//go:build !solution

package main

import (
	"flag"
	"image"
	"image/color"
	"image/png"
	"log"
	"net/http"
	"strconv"
	"strings"
	"time"
)

func main() {
	port := flag.Int("port", 8000, "Port to listen on")
	flag.Parse()
	http.HandleFunc("/", hand)
	log.Fatal(http.ListenAndServe("localhost:"+strconv.Itoa(*port), nil))
}

func hand(w http.ResponseWriter, req *http.Request) {
	example := "02:39:39"
	k, err := strconv.Atoi(req.URL.Query().Get("k"))
	timeStr := req.URL.Query().Get("time")
	//_, _ := time.Parse(example, timeStr)
	if timeStr == "" {
		timeStr = time.Now().Format(example)
	}
	// hours, _ := strconv.Atoi(strings.Split(timeStr, ":")[0])
	// min, _ := strconv.Atoi(strings.Split(timeStr, ":")[1])
	// sec, _ := strconv.Atoi(strings.Split(timeStr, ":")[2])
	if k < 1 || k > 30 || err != nil || len(timeStr) != 8 {
		http.Error(w, "invalid k", http.StatusBadRequest)
		return
	}

	height := len(strings.Split(Zero, "\n"))
	width := (Width(Zero)*6 + Width(Colon)*2)
	img := image.NewRGBA(image.Rect(0, 0, width*k, height*k))
	drawTime(timeStr, k, img)
	err = png.Encode(w, img)
	if err != nil {
		w.WriteHeader(http.StatusInternalServerError)
		return
	} else {
		w.Header().Set("Content-Type", "image/png")
		w.WriteHeader(http.StatusOK)
	}
}

func drawTime(timeString string, k int, img *image.RGBA) {
	startX := 0
	startY := 0
	for _, ch := range timeString {
		x := startX
		y := startY
		symbol := getSymbol(string(ch))
		localWid := Width(symbol)
		for _, sym := range symbol {
			if sym == '\n' {
				x = startX
				y += 1
				continue
			}
			for i := 0; i < k; i++ {
				for j := 0; j < k; j++ {
					if sym == '1' {
						img.Set(x*k+j, y*k+i, Cyan)
					} else if sym == '.' {
						img.Set(x*k+j, y*k+i, color.White)
					}
				}
			}
			x += 1
		}
		startY = 0
		startX += localWid
	}
}

func getSymbol(s string) string {
	switch s {
	case "0":
		return Zero
	case "1":
		return One
	case "2":
		return Two
	case "3":
		return Three
	case "4":
		return Four
	case "5":
		return Five
	case "6":
		return Six
	case "7":
		return Seven
	case "8":
		return Eight
	case "9":
		return Nine
	case ":":
		return Colon
	default:
		return ""
	}
}

func Width(s string) int {
	return len(strings.SplitN(s, "\n", 2)[0])
}
