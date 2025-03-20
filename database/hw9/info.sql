CREATE TABLE Flights (
    FlightId      SERIAL,
    FlightTime    TIMESTAMP NOT NULL,
    PlaneId       INT NOT NULL,
    isAllowedBuy  BOOLEAN NOT NULL,
    isAllowedReserv BOOLEAN NOT NULL,
    PRIMARY KEY (FlightId)
);

CREATE TABLE Seats (
    PlaneId     INT NOT NULL,
    SeatNo      VARCHAR(4) NOT NULL,
    PRIMARY KEY (PlaneId, SeatNo)
);

CREATE TABLE Passenger (
    UserId      INT PRIMARY KEY,
    Pass        TEXT NOT NULL
);

CREATE TABLE Tickets (
    TicketId    SERIAL,
    FlightId    INT NOT NULL,
    PlaneId     INT NOT NULL,
    SeatNo      VARCHAR(4) NOT NULL,
    UserId      INT NULL,  
    isBought    BOOLEAN NOT NULL,
    TimeTo      TIMESTAMP,
    
    FOREIGN KEY (FlightId) REFERENCES Flights(FlightId),
    FOREIGN KEY (PlaneId, SeatNo) REFERENCES Seats(PlaneId, SeatNo),
    FOREIGN KEY (UserId) REFERENCES Passenger(UserId)
);
