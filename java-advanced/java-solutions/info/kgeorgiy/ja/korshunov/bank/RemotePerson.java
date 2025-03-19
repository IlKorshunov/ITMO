package info.kgeorgiy.ja.korshunov.bank;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;

public class RemotePerson extends AbstractPerson {
    private final int port;
    public RemotePerson(String name, String surname, String passport, int port) throws RemoteException {
        super(name, surname, passport);
        this.port = port;
        UnicastRemoteObject.exportObject(this, port);
    }

    public synchronized Account createAccount(final String id) throws RemoteException {
        Account account = super.createAccount(id);
        UnicastRemoteObject.exportObject(account, port);
        return account;
    }

}
