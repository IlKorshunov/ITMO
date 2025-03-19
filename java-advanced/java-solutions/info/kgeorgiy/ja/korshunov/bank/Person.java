package info.kgeorgiy.ja.korshunov.bank;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface Person extends Remote {
    String GetName() throws RemoteException;
    String GetSurname() throws RemoteException;
    String GetPassport()throws RemoteException;

    Account createAccount(final String id) throws RemoteException;
    Account getAccount(final String id) throws RemoteException;
    
}
