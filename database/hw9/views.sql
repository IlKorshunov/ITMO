CREATE OR REPLACE VIEW ReservedSeats AS
SELECT FlightId, PlaneId, SeatNo, UserId, TimeTo
FROM Tickets
WHERE isBought = FALSE;


CREATE OR REPLACE VIEW BoughtSeats AS
SELECT FlightId, PlaneId, SeatNo, UserId
FROM Tickets
WHERE isBought = TRUE;

CREATE OR REPLACE VIEW FreeSeats AS
SELECT f.FlightId, s.PlaneId, s.SeatNo
FROM Flights f
JOIN Seats s ON s.PlaneId = f.PlaneId
WHERE NOT EXISTS (
    SELECT 1
    FROM Tickets t
    WHERE t.FlightId = f.FlightId
      AND t.PlaneId = s.PlaneId
      AND t.SeatNo = s.SeatNo
);
