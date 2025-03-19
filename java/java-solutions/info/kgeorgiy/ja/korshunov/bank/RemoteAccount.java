package info.kgeorgiy.ja.korshunov.bank;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;

public class RemoteAccount extends AbstractAccount {
    private final int port;
    public RemoteAccount(String id, int port) throws RemoteException {
        super(id);
        this.port = port;
        UnicastRemoteObject.exportObject(this, port);
    }
}
