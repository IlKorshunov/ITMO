package info.kgeorgiy.ja.korshunov.bank;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface Bank extends Remote {
    Person getPerson(String passport, Class<? extends Person> personClass) throws RemoteException;

    void AddPerson(String name, String surname, String passport) throws RemoteException;
}
