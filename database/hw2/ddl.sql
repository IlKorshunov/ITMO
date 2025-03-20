CREATE TABLE Groups (
    id int GENERATED ALWAYS AS IDENTITY primary key,
	group_name varchar(8) not null,
	unique (group_name)
)

CREATE TABLE Student (
    id int GENERATED ALWAYS AS IDENTITY primary key,
	student_firstname varchar(25) not null,
	student_secondname varchar(25) not null,
    group_id int,
    FOREIGN KEY (group_id) REFERENCES Groups(id)
)

CREATE TABLE Lecturer (
    id int GENERATED ALWAYS AS IDENTITY primary key,
    lecturer_firstname varchar(25) not null,
	lecturer_secondname varchar(25) not null,
    years_at_university int NOT NULL CHECK (years_at_university >= 0 AND years_at_university <= 60)
)

CREATE TABLE Course (
    id int GENERATED ALWAYS AS IDENTITY primary key,
    name varchar(10) not null,
    term int NOT NULL CHECK (term >= 0 AND term <= 8)
)

CREATE TABLE MemberStudent (
    student_id int NOT NULL,
    course_id int NOT NULL,
    points int NOT NULL CHECK (points >= 0 AND points <= 100), 
    PRIMARY KEY (student_id, course_id),
    FOREIGN KEY (student_id) REFERENCES Student(id),
    FOREIGN KEY (course_id) REFERENCES Course(id)
)

CREATE TABLE MemberLecturer (
    teacher_id int NOT NULL,
    course_id int NOT NULL,
    is_main_at_course boolean NOT NULL, 
    PRIMARY KEY (teacher_id, course_id),
    FOREIGN KEY (teacher_id) REFERENCES Lecturer(id),
    FOREIGN KEY (course_id) REFERENCES Course(id)
)