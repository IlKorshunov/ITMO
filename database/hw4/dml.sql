INSERT INTO Groups (group_name) VALUES 
('M3332'),
('M3333'),
('M3333');

INSERT INTO Student (Name, GroupId) VALUES 
('Иван Иванов', 1),
('Анна Петрова', 2),
('Ольга Смирнова', 1),
('Павел Сидоров', 3);

INSERT INTO Course (Name) VALUES 
('Math'),
('FP'),
('DA'),
('History');

INSERT INTO Lecturer (Name) VALUES 
('Андрей Станкевич'),
('Георгий Корнеев'),
('Николай Ведерников'),
('Дмитрий Штукенберг');

INSERT INTO Mark (CourseId, StudentId, Mark) VALUES 
(1, 1, 85),  
(1, 2, 90),  
(2, 1, 78),
(2, 3, 88),  
(3, 4, 92);  

INSERT INTO WorkLecturer (GroupId, CourseId, LecturerId) VALUES 
(1, 1, 1),
(2, 2, 2),
(3, 3, 3),
(1, 4, 4);  

-- SELECT * FROM Student;
-- SELECT * FROM Groups;
-- SELECT * FROM Course;
-- SELECT * FROM Lecturer;
-- SELECT * FROM Mark;
-- SELECT * FROM WorkLecturer;