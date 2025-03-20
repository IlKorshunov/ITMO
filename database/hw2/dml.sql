INSERT INTO Groups (group_name) VALUES
    ('M3432'),
    ('M3433'),
    ('M3434'),
    ('M3435');


INSERT INTO Student (student_firstname, student_secondname, group_id) VALUES
    ('Ivan', 'Ivanov', 1),
    ('Petr', 'Petrov', 1),
    ('Anna', 'Smirnova', 2),
    ('Olga', 'Kuznetsova', 3);


INSERT INTO Lecturer (lecturer_firstname, lecturer_secondname, years_at_university) VALUES
    ('Georgiy', 'Korneev', 25),
    ('Nikolay', 'Vedernikov', 15),
    ('Andrew', 'Stankevich', 25);

INSERT INTO Course (name, term) VALUES
    ('Math', 1),
    ('Linal', 1),
    ('Java', 1)
    ('FP', 5);

INSERT INTO MemberStudent (student_id, course_id, points) VALUES
    (1, 1, 85),
    (1, 2, 90),
    (2, 1, 78),
    (3, 3, 95),
    (4, 2, 60);

INSERT INTO MemberLecturer (teacher_id, course_id, is_main_at_course) VALUES
    (1, 3, TRUE),   
    (2, 3, True),   
    (3, 3, True), 