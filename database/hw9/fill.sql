INSERT INTO Flights (FlightTime, PlaneId, isAllowedBuy, isAllowedReserv)
VALUES
  (NOW() + INTERVAL '5 days', 100, TRUE, TRUE),
  (NOW() + INTERVAL '2 days', 101, TRUE, TRUE),
  (NOW() + INTERVAL '2 hours', 102, TRUE, TRUE);

INSERT INTO Seats (PlaneId, SeatNo)
VALUES
  (100, '1A'),
  (100, '1B'),
  (100, '1C'),
  (101, '1A'),
  (101, '1B'),
  (102, '1A'),
  (102, '1B'),
  (102, '1C'),
  (102, '1D');

INSERT INTO Passenger (UserId, Pass)
VALUES
  (1, 'pass1'),
  (2, 'pass2'),
  (3, 'pass3');

INSERT INTO Tickets (FlightId, PlaneId, SeatNo, UserId, isBought, TimeTo)
VALUES 
    (1, 100, '1A', 1, FALSE, NOW() + INTERVAL '1 day'),
    (1, 100, '1B', 2, TRUE, NULL),
    (2, 101, '1A', 3, TRUE, NULL);
