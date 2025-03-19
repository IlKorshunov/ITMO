//go:build !solution

package main

import (
	"encoding/json"
	"flag"
	"log"
	"math/rand"
	"net/http"
	"strconv"
)

var KeyToURL map[string]string
var URLToKey map[string]string

type ShortenRequest struct {
	URL string `json:"url"`
}

type Answer struct {
	Key string `json:"key"`
	URL string `json:"url"`
}

func generateKey() string {
	return strconv.Itoa(rand.Intn(1e9))
}

func PostShorten(w http.ResponseWriter, r *http.Request, req ShortenRequest) {
	if err := json.NewDecoder(r.Body).Decode(&req); err != nil {
		http.Error(w, "JSON incorrect", http.StatusBadRequest)
		return
	}
	key, ok := URLToKey[req.URL]
	response := Answer{
		URL: req.URL,
	}
	if ok {
		response.Key = key
	} else {
		key = generateKey()
		KeyToURL[key] = req.URL
		URLToKey[req.URL] = key
		response.Key = key
	}
	jsonData, err := json.Marshal(response)
	if err != nil {
		http.Error(w, "Error marshaling JSON", http.StatusInternalServerError)
		return
	}
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusOK)
	_, err = w.Write(jsonData)
	if err != nil {
		http.Error(w, "Error during write", http.StatusInternalServerError)
		return
	}
}

func GetGo(w http.ResponseWriter, r *http.Request) {
	key := r.URL.String()[4:]
	URL, ok := KeyToURL[key]
	if ok {
		http.Redirect(w, r, URL, http.StatusFound)
	} else {
		http.Error(w, "key does not exist", http.StatusNotFound)
		return
	}
}

func main() {
	port := flag.Int("port", 8002, "Port to listen on")
	flag.Parse()
	var req ShortenRequest
	KeyToURL = make(map[string]string)
	URLToKey = make(map[string]string)
	http.HandleFunc("/shorten", func(w http.ResponseWriter, r *http.Request) {
		PostShorten(w, r, req)
	})
	http.HandleFunc("/go/", GetGo)
	log.Fatal(http.ListenAndServe("localhost:"+strconv.Itoa(*port), nil))
}
