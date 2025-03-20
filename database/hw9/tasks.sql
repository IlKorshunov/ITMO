-- helping functions

CREATE OR REPLACE FUNCTION commonOperatin(
    p_FlightId INT,
    p_SeatNo VARCHAR(4),
    p_threshold INTERVAL,  
    p_operation TEXT
)
RETURNS BOOLEAN 
LANGUAGE plpgsql
AS $$
DECLARE 
    _FlightTime TIMESTAMP;
    flag BOOLEAN;
BEGIN
    IF EXISTS (
        SELECT 1 
        FROM Tickets 
        WHERE FlightId = p_FlightId 
          AND SeatNo = p_SeatNo
    ) THEN
        IF p_operation = 'BUY' OR p_operation = 'RESERVE' THEN
            RETURN FALSE;
        END IF;
    END IF;

    IF p_operation = 'BUY' OR p_operation = 'BUY_RESERVED' THEN
        SELECT FlightTime, isAllowedBuy
          INTO _FlightTime, flag
          FROM Flights
          WHERE FlightId = p_FlightId;
    ELSIF p_operation = 'RESERVE' OR p_operation = 'EXTEND' THEN
        SELECT FlightTime, isAllowedReserv
          INTO _FlightTime, flag
          FROM Flights
          WHERE FlightId = p_FlightId;
    ELSE
        RETURN FALSE;
    END IF;


    IF NOT flag THEN
        RETURN FALSE;
    END IF;

    IF (NOW() + p_threshold) >= _FlightTime THEN
        RETURN FALSE;
    END IF;

    RETURN TRUE;
END;
$$;

CREATE OR REPLACE FUNCTION CanBuySeat(p_FlightId INT, p_SeatNo VARCHAR(4))
RETURNS BOOLEAN 
LANGUAGE plpgsql
AS $$
BEGIN
    RETURN commonOperatin(p_FlightId, p_SeatNo, INTERVAL '3 hours', 'BUY');
END;
$$;

CREATE OR REPLACE FUNCTION CanReserveSeat(p_FlightId INT, p_SeatNo VARCHAR(4))
RETURNS BOOLEAN 
LANGUAGE plpgsql
AS $$
BEGIN
    RETURN commonOperatin(p_FlightId, p_SeatNo, INTERVAL '3 days', 'RESERVE');
END;
$$;

CREATE OR REPLACE FUNCTION CanExtendReserv(p_FlightId INT, p_SeatNo VARCHAR(4))
RETURNS BOOLEAN 
LANGUAGE plpgsql
AS $$
BEGIN
    RETURN commonOperatin(p_FlightId, p_SeatNo, INTERVAL '3 days', 'EXTEND');
END;
$$;

CREATE OR REPLACE FUNCTION CanBuyReserved(p_FlightId INT, p_SeatNo VARCHAR(4))
RETURNS BOOLEAN 
LANGUAGE plpgsql
AS $$
BEGIN
    RETURN commonOperatin(p_FlightId, p_SeatNo, INTERVAL '3 hours', 'BUY_RESERVED');
END;
$$;


CREATE OR REPLACE FUNCTION CheckUser(_UserId INT, _Pass TEXT)
RETURNS BOOLEAN
LANGUAGE plpgsql
AS $$
BEGIN
    IF NOT EXISTS (
        SELECT 1
        FROM Passenger
        WHERE UserId = _UserId
          AND Pass = _Pass
    ) THEN
        RETURN FALSE;
    END IF;

    RETURN TRUE;
END;
$$;

CREATE OR REPLACE FUNCTION CheckFlight(_FlightId integer)
RETURNS boolean
LANGUAGE plpgsql
AS $$
BEGIN
    IF EXISTS (
        SELECT 1
        FROM Flights
        WHERE FlightId = _FlightId
    ) THEN 
        RETURN TRUE;
    END IF;
    RETURN FALSE;
END;
$$;

CREATE OR REPLACE FUNCTION CheckTicket(_FlightId integer, _SeatNo varchar(4))
RETURNS boolean
LANGUAGE plpgsql
AS $$
DECLARE
    v_PlaneId INT;
BEGIN
    SELECT PlaneId
      INTO v_PlaneId
      FROM Flights
      WHERE FlightId = _FlightId;
      
    IF NOT FOUND THEN
         RETURN FALSE;
    END IF;

    IF NOT EXISTS (
         SELECT 1 
         FROM Seats 
         WHERE PlaneId = v_PlaneId 
           AND SeatNo = _SeatNo
    ) THEN
         RETURN FALSE;
    END IF;

    IF EXISTS (
         SELECT 1 
         FROM Tickets 
         WHERE FlightId = _FlightId 
           AND SeatNo = _SeatNo
    ) THEN
         RETURN FALSE;
    END IF;

    RETURN TRUE;
END;
$$;


-- main functions

CREATE OR REPLACE FUNCTION RegisterUser(p_UserId INT, p_Pass TEXT)
RETURNS BOOLEAN
LANGUAGE plpgsql
AS $$
BEGIN
    IF EXISTS (
        SELECT 1
        FROM Passenger p
        WHERE p.UserId = p_UserId
          AND p.Pass = p_Pass
    ) THEN
        RETURN FALSE;
    END IF;

    INSERT INTO Passenger (UserId, Pass)
    VALUES (p_UserId, p_Pass);

    RETURN TRUE;
END;
$$;


CREATE OR REPLACE FUNCTION ManageFlight(
    _UserId INT,
    _Pass TEXT,
    _FlightId INT,
    _SellAllowed BOOLEAN,
    _ReservationAllowed BOOLEAN
) 
RETURNS BOOLEAN
LANGUAGE plpgsql
AS $$
DECLARE
    _FlightTime TIMESTAMP;
    v_current_time TIMESTAMP;
BEGIN
    IF NOT CheckFlight(_FlightId) THEN
        RETURN FALSE;
    END IF;

    IF NOT CheckUser(_UserId, _Pass) THEN
        RETURN FALSE;
    END IF;

    SELECT FlightTime
      INTO _FlightTime
      FROM Flights
      WHERE FlightId = _FlightId;

    IF NOT FOUND THEN
        RETURN FALSE;
    END IF;

    v_current_time := NOW();

    IF (v_current_time + INTERVAL '3 hours') >= _FlightTime THEN
        _SellAllowed := FALSE;
    END IF;

    IF (v_current_time + INTERVAL '3 days') >= _FlightTime THEN
        _ReservationAllowed := FALSE;
    END IF;

    UPDATE Flights
    SET isAllowedBuy    = _SellAllowed,
        isAllowedReserv = _ReservationAllowed
    WHERE FlightId = _FlightId;

    RETURN TRUE;
END;
$$;


CREATE OR REPLACE FUNCTION FreeSeats(_FlightId INT)
RETURNS TABLE (SeatNo VARCHAR(4))
LANGUAGE plpgsql
AS $$
BEGIN
    RETURN QUERY
    SELECT s.SeatNo
    FROM Seats s
         JOIN Flights f ON s.PlaneId = f.PlaneId
         LEFT JOIN Tickets t ON t.FlightId = f.FlightId
                            AND t.PlaneId  = s.PlaneId
                            AND t.SeatNo   = s.SeatNo
    WHERE f.FlightId = _FlightId AND t.TicketId IS NULL
;
END;
$$;


CREATE OR REPLACE FUNCTION Reserve(_UserId INT, _Pass TEXT, _FlightId INT, _SeatNo VARCHAR(4))
RETURNS BOOLEAN
LANGUAGE plpgsql
AS $$
DECLARE
    v_PlaneId INT;
BEGIN
    IF NOT CheckUser(_UserId, _Pass) THEN
        RETURN FALSE;
    END IF;

    IF NOT CheckFlight(_FlightId) THEN
        RETURN FALSE;
    END IF;

    IF NOT CanReserveSeat(_FlightId, _SeatNo) THEN
        RETURN FALSE;
    END IF;

    SELECT PlaneId
      INTO v_PlaneId
      FROM Flights
      WHERE FlightId = _FlightId;

    INSERT INTO Tickets (FlightId, PlaneId, SeatNo, UserId, isBought, TimeTo)
    VALUES (_FlightId, v_PlaneId, _SeatNo, _UserId, FALSE, NOW() + INTERVAL '1 day');

    RETURN TRUE;
END;
$$;

CREATE OR REPLACE FUNCTION ExtendReservation(_UserId int, _Pass Text, _FlightId int, _SeatNo VARCHAR(4))
RETURNS BOOLEAN
LANGUAGE plpgsql
AS $$
DECLARE
    v_PlaneId INT;
BEGIN
    IF NOT CheckUser(_UserId, _Pass) THEN
        RETURN FALSE;
    END IF;

    IF NOT CheckFlight(_FlightId) THEN
        RETURN FALSE;
    END IF;

    IF NOT CanExtendReserv(_FlightId, _SeatNo) THEN
        RETURN FALSE;
    END IF;

    SELECT PlaneId
      INTO v_PlaneId
      FROM Flights
      WHERE FlightId = _FlightId;

    UPDATE Tickets
    SET TimeTo = NOW() + INTERVAL '1 day'
    WHERE FlightId = _FlightId
      AND SeatNo = _SeatNo
      AND UserId = _UserId
      AND isBought = FALSE;

    IF FOUND THEN
        RETURN TRUE;
    ELSE
        RETURN FALSE;
    END IF;
END;
$$;


CREATE OR REPLACE FUNCTION BuyFree(_FlightId INT, _SeatNo VARCHAR(4))
RETURNS BOOLEAN
LANGUAGE plpgsql
AS $$
DECLARE
    v_PlaneId INT;
BEGIN
    IF NOT CheckTicket(_FlightId, _SeatNo) THEN
        RETURN FALSE;
    END IF;
    
    SELECT PlaneId
      INTO v_PlaneId
      FROM Flights
      WHERE FlightId = _FlightId;
    
    INSERT INTO Tickets (FlightId, PlaneId, SeatNo, UserId, isBought, TimeTo)
    VALUES (_FlightId, v_PlaneId, _SeatNo, NULL, TRUE, NULL);
    
    RETURN TRUE;
END;
$$;


CREATE OR REPLACE FUNCTION BuyReserved(_UserId INT, _Pass TEXT, _FlightId INT, _SeatNo VARCHAR(4))
RETURNS BOOLEAN
LANGUAGE plpgsql
AS $$
DECLARE
    v_PlaneId INT;
BEGIN
    IF NOT CheckUser(_UserId, _Pass) THEN
        RETURN FALSE;
    END IF;
    
    IF NOT CheckFlight(_FlightId) THEN
        RETURN FALSE;
    END IF;
    
    IF NOT EXISTS (
        SELECT 1 
        FROM Tickets
        WHERE FlightId = _FlightId
          AND SeatNo = _SeatNo
          AND isBought = FALSE
          AND UserId = _UserId
    ) THEN
        RETURN FALSE;
    END IF;
    
    SELECT PlaneId
      INTO v_PlaneId
      FROM Flights
      WHERE FlightId = _FlightId;
    
    UPDATE Tickets
    SET isBought = TRUE,
        TimeTo = NULL
    WHERE FlightId = _FlightId
      AND SeatNo = _SeatNo
      AND UserId = _UserId;
    
    IF FOUND THEN
        RETURN TRUE;
    ELSE
        RETURN FALSE;
    END IF;
END;
$$;


-- statistic

CREATE OR REPLACE FUNCTION FlightStat(_UserId INT, _Pass TEXT, _FlightId INT)
RETURNS TABLE (
    FlightId INT,
    CanReserve BOOLEAN,
    CanBuy BOOLEAN,
    FreeSeats INT,
    ReservedSeats INT,
    SoldSeats INT
)
LANGUAGE plpgsql
AS $$
DECLARE
    v_FlightTime         TIMESTAMP;
    v_isAllowedBuy       BOOLEAN;
    v_isAllowedReserv    BOOLEAN;
    v_current_time       TIMESTAMP;
    v_freeSeats          INT;
    v_reservedSeats      INT;
    v_soldSeats          INT;
    v_PlaneId            INT;
BEGIN
    IF NOT CheckUser(_UserId, _Pass) THEN
        RETURN;
    END IF;

    SELECT f.FlightTime, f.isAllowedBuy, f.isAllowedReserv, f.PlaneId
      INTO v_FlightTime, v_isAllowedBuy, v_isAllowedReserv, v_PlaneId
      FROM Flights f
      WHERE f.FlightId = _FlightId;
      
    IF NOT FOUND THEN
         RETURN;
    END IF;

    v_current_time := NOW();

    SELECT COUNT(*)
      INTO v_freeSeats
      FROM Seats s
      WHERE s.PlaneId = v_PlaneId
        AND NOT EXISTS (
            SELECT 1 
            FROM Tickets t
            WHERE t.FlightId = _FlightId
              AND t.PlaneId = s.PlaneId
              AND t.SeatNo = s.SeatNo
        );

    SELECT COUNT(*)
      INTO v_reservedSeats
      FROM Tickets t
      WHERE t.FlightId = _FlightId
        AND t.isBought = FALSE;

    SELECT COUNT(*)
      INTO v_soldSeats
      FROM Tickets t
      WHERE t.FlightId = _FlightId
        AND t.isBought = TRUE;

    IF (v_current_time + INTERVAL '3 days' < v_FlightTime) AND (v_freeSeats >= 1) THEN
        v_isAllowedReserv := TRUE;
    ELSE
        v_isAllowedReserv := FALSE;
    END IF;

    IF (v_current_time + INTERVAL '3 hours' < v_FlightTime) AND (v_freeSeats >= 1) THEN
        v_isAllowedBuy := TRUE;
    ELSE
        v_isAllowedBuy := FALSE;
    END IF;

    RETURN QUERY
      SELECT _FlightId,
             v_isAllowedReserv AS CanReserve,
             v_isAllowedBuy AS CanBuy,
             v_freeSeats AS FreeSeats,
             v_reservedSeats AS ReservedSeats,
             v_soldSeats AS SoldSeats;
END;
$$;


CREATE OR REPLACE FUNCTION FlightsStatistics(_UserId INT, _Pass TEXT)
RETURNS TABLE (
    TotalFreeSeats     INT,
    TotalReservedSeats INT,
    TotalSoldSeats     INT,
    CanBuy             BOOLEAN,
    CanReserve         BOOLEAN
)
LANGUAGE plpgsql
AS $$
DECLARE
    rec RECORD;
    stat_rec RECORD;
    v_freeSeats     INT := 0;
    v_reservedSeats INT := 0;
    v_soldSeats     INT := 0;
    v_canBuy        BOOLEAN := FALSE;
    v_canReserve    BOOLEAN := FALSE;
BEGIN
    IF NOT CheckUser(_UserId, _Pass) THEN
        RETURN;
    END IF;
    
    FOR rec IN
         SELECT FlightId FROM Flights
    LOOP
         FOR stat_rec IN
             SELECT * FROM FlightStat(_UserId, _Pass, rec.FlightId)
         LOOP
             v_freeSeats     := v_freeSeats + stat_rec.FreeSeats;
             v_reservedSeats := v_reservedSeats + stat_rec.ReservedSeats;
             v_soldSeats     := v_soldSeats + stat_rec.SoldSeats;

             IF stat_rec.CanBuy THEN
                 v_canBuy := TRUE;
             END IF;
             IF stat_rec.CanReserve THEN
                 v_canReserve := TRUE;
             END IF;
         END LOOP;
    END LOOP;
    
    RETURN QUERY
    SELECT v_freeSeats, v_reservedSeats, v_soldSeats, v_canBuy, v_canReserve;
END;
$$;
