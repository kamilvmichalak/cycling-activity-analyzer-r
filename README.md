# Cycling Activity Analyzer R

## Opis projektu

Cycling Activity Analyzer R to lokalna aplikacja Shiny do analizy aktywności rowerowych zapisanych w plikach FIT. Po wskazaniu pliku aplikacja importuje rekordy do `data.frame`, przygotowuje dane i udostępnia podsumowanie treningu, wykresy, mapę trasy, segmenty oraz strefy tętna.

Plik użytkownika jest przetwarzany lokalnie. Repozytorium nie przechowuje prywatnych aktywności.

## Funkcjonalności

- wybór i walidacja lokalnego pliku `.fit`,
- import rekordów FIT do ustandaryzowanego `data.frame`,
- obsługa brakujących pól, wartości `NA` i nieprawidłowych współrzędnych,
- obliczanie czasu, dystansu, prędkości i sumy podjazdów,
- podsumowanie aktywności,
- wykresy prędkości, tętna, wysokości i śladu GPS,
- interaktywna mapa trasy,
- segmenty dystansowe i czasowe,
- strefy tętna wyznaczane na podstawie HRmax.

## Struktura katalogów

```text
cycling-activity-analyzer-r/
├── app.R                         # aplikacja Shiny
├── DESCRIPTION                   # zależności projektu
├── R/                            # funkcje importu i analizy
├── data/raw/                     # lokalne dane wejściowe (ignorowane)
├── data/processed/               # lokalne dane przetworzone (ignorowane)
├── tests/manual_checklist.md     # lista testów ręcznych
└── docs/screenshots/             # miejsce na zrzuty ekranu
```

Najważniejsze moduły w katalogu `R/`:

- `import_fit.R` — import i standaryzacja danych FIT,
- `prepare_data.R` — czyszczenie danych oraz przeliczanie jednostek,
- `summary.R` — metryki zbiorcze,
- `plots.R` — wykresy i mapa,
- `segments.R` — segmentacja aktywności,
- `heart_zones.R` — strefy tętna,
- `mock_data.R` — sztuczne dane do testów.

## Wymagania

- R w wersji co najmniej 4.1.0,
- pakiety: `shiny`, `ggplot2`, `dplyr`, `lubridate`, `DT`, `leaflet`,
- pakiet `FITfileR` instalowany z GitHuba.

## Instalacja zależności

W konsoli R wykonaj:

```r
install.packages(c(
  "shiny", "ggplot2", "dplyr", "lubridate",
  "DT", "leaflet", "remotes"
))

remotes::install_github("grimbough/FITfileR")
```

## Uruchomienie

Otwórz projekt w RStudio albo ustaw katalog roboczy na katalog repozytorium, a następnie wykonaj:

```r
shiny::runApp()
```

Po uruchomieniu:

1. Kliknij przycisk wyboru pliku w lewym panelu.
2. Wskaż aktywność z rozszerzeniem `.fit` lub `.FIT`.
3. Poczekaj na import danych.
4. Przejdź między zakładkami podsumowania, danych, wykresów, segmentów i stref tętna.
5. Ustaw długość segmentu oraz własne tętno maksymalne.

## Dane wejściowe

Pliki FIT mogą zawierać między innymi czas, dystans, prędkość, współrzędne GPS, wysokość, tętno, kadencję, moc i temperaturę. Aplikacja zachowuje dodatkowe pola udostępnione przez urządzenie, ale jej podstawowe moduły korzystają ze wspólnego zestawu kolumn.

Prywatne pliki `.fit` są ignorowane przez Git niezależnie od miejsca zapisania w projekcie. Katalogi `data/raw/` i `data/processed/` służą wyłącznie do lokalnej pracy.

## Ograniczenia

- nie każdy plik FIT zawiera komplet danych,
- sposób zapisu pól może zależeć od producenta urządzenia,
- kafelki interaktywnej mapy wymagają połączenia z internetem; prosty wykres GPS działa bez nich,
- bardzo duże pliki mogą wymagać dłuższego czasu importu.

Brak GPS, tętna, wysokości, mocy lub kadencji nie powinien zatrzymać aplikacji. Niedostępne wyniki są zastępowane komunikatem dla użytkownika.

## Autorzy

- Kamil Michalak
- Marcin Szaroleta
- Kacper Michałowski
