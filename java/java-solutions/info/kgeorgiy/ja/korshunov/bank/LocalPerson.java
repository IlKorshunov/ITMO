package info.kgeorgiy.ja.korshunov.bank;

import info.kgeorgiy.ja.korshunov.bank.AbstractPerson;

import java.io.Serializable;

public class LocalPerson extends AbstractPerson implements Serializable {
    public LocalPerson(String name, String surname, String passport) {
        super(name, surname, passport);
    }

}
