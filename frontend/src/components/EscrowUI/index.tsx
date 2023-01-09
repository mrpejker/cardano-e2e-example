import React, { useState } from "react"
import { Container, Navbar, Nav, Button, Modal, Form, Table, Spinner } from "react-bootstrap";
import Row from 'react-bootstrap/Row';
import Col from 'react-bootstrap/Col';
import { UserEndpoints, ObsState, UtxoEscrowInfo } from "src/contractEndpoints/escrow";
import { mkStartParams, mkCancelParams, mkResolveParams } from "src/contractEndpoints/parameters";
import type { TxOutRef } from "cardano-pab-client";

// Main component for the EscrowUI. It includes all the other components.
function EscrowUI() {
  const [currentContractState, setCurrentContractState] = useState<ObsState>([])
  const [contractEndpoints, setContractEndpoints] = useState<UserEndpoints>(new UserEndpoints([]));
  const [isConnected, setIsConnected] = useState(false);
  const [showStartModal, setShowStartModal] = useState(false);
  const [showCancelModal, setShowCancelModal] = useState(false);

  const handleShowStart = () => setShowStartModal(true);
  const handleShowCancel = () => setShowCancelModal(true);
  return (
    <Container>
      <Navbar sticky="top" bg="light">
        <Navbar.Brand><h1>Exchange Escrow</h1></Navbar.Brand>
        <Connect
          setCurrentContractState={setCurrentContractState}
          contractEndpoints={contractEndpoints}
          setIsConnected={setIsConnected}
          setContractEndpoints={setContractEndpoints}
        />
      </Navbar>
      <br></br>
      <Button
        style={{ marginRight: "20px" }}
        variant="primary"
        size="lg"
        onClick={handleShowStart}
        disabled={!isConnected}
      >
        Start new Escrow
      </Button>
      <Start
        showStartModal={showStartModal}
        setShowStartModal={setShowStartModal}
        contractEndpoints={contractEndpoints}
      />

      <Button
        style={{ marginRight: "20px" }}
        variant="primary"
        size="lg"
        onClick={handleShowCancel}
        disabled={!isConnected}
      >
        Cancel an Escrow
      </Button>
      <Cancel
        showCancelModal={showCancelModal}
        setShowCancelModal={setShowCancelModal}
        contractEndpoints={contractEndpoints}
      ></Cancel>
      <br></br>
      <br></br>
      <h2> Escrows to Resolve </h2>
      <br></br>
      <ContractInformation
        currentContractState={currentContractState}
        contractEndpoints={contractEndpoints}
      />
      <Reload
        contractEndpoints={contractEndpoints}
        isConnected={isConnected}
        setCurrentContractState={setCurrentContractState}
      />

    </Container>
  )
}

type ConnectProps = {
  setCurrentContractState: React.Dispatch<React.SetStateAction<ObsState>>
  contractEndpoints: UserEndpoints
  setIsConnected: React.Dispatch<React.SetStateAction<boolean>>
  setContractEndpoints: React.Dispatch<React.SetStateAction<UserEndpoints>>
};
// Connect component that handles the wallet and endpoint connection.
const Connect = ({ setCurrentContractState, contractEndpoints, setIsConnected, setContractEndpoints }: ConnectProps) => {
  const [selectedWallet, setSelectedWallet] = useState<"eternl" | "nami">("eternl");
  return (
    <Navbar.Collapse className="justify-content-end">
      <Nav.Link className="d-flex">
        <select
          style={{ marginRight: "16px" }}
          id="comboA"
          onChange={e => {
            console.log("Value Changed")
            console.log(e.target.value)
            setIsConnected(false)
            setSelectedWallet(e.target.value as "eternl" | "nami")
          }}
          defaultValue={"eternl"}
        >
          <option value="nami">Nami</option>
          <option value="eternl">Eternl</option>
        </select>
        <ButtonWithSpinner
            onClick={ async () => {
              console.log("Connecting")
              const ce = await contractEndpoints.connect(selectedWallet)
              console.log("Connected")
              setContractEndpoints(ce)
              setIsConnected(true)
              const obsState = await ce.reload()
              setCurrentContractState(obsState)}
            }
            isDisabled={false}
            text={"Connect Wallet"}
        />
      </Nav.Link>
    </Navbar.Collapse>
  )
}

// Reload component that reloads the contract state
const Reload = ({contractEndpoints, isConnected, setCurrentContractState}) => {
  return (
    <ButtonWithSpinner
      onClick={async () => {
        console.log("Reloading")
        const obsState = await contractEndpoints.reload()
        setCurrentContractState(obsState)
      }}
      isDisabled={!isConnected}
      text={"Reload"}
    />
  )
}

type StartProps = {
  showStartModal: boolean
  setShowStartModal: React.Dispatch<React.SetStateAction<boolean>>
  contractEndpoints: UserEndpoints
};
// Component that displays the form for starting a new Escrow.
const Start = ({ showStartModal, setShowStartModal, contractEndpoints }: StartProps) => {

  const handleClose = e => {
    setShowStartModal(false)
    e.preventDefault()
    const formData = new FormData(e.target),
      recAddr = formData.get("recAddr") as string,
      sendCurrency = formData.get("sendCurrency") as string,
      sendTN = formData.get("sendTokenName") as string,
      sendAmount = parseInt(formData.get("sendAmount") as string),
      recCurrency = formData.get("recCurrency") as string,
      recTN = formData.get("recTokenName") as string,
      recAmount = parseInt(formData.get("recAmount") as string)

    mkStartParams(recAddr, sendCurrency, sendTN, sendAmount, recCurrency,
      recTN, recAmount)
      .then(sp => contractEndpoints.start(sp))
  }
  return (
    <>
      <Modal show={showStartModal} size="lg">
        <Modal.Header closeButton onHide={ () => setShowStartModal(false)}>
          <Modal.Title>Start new Escrow</Modal.Title>
        </Modal.Header>
        <Modal.Body>
          <Form onSubmit={handleClose}>
            <Form.Group className="mb-3" controlId="startForm">
              <Form.Label>Receiver Address</Form.Label>
              <Form.Control
                name="recAddr"
                placeholder="Address"
                autoFocus
              />
              <br></br>
              <div className="position-relative">
                <Row>
                  <Col>
                    <div className='d-flex align-items-center' style={{ border: '1px solid black', borderRadius: '1rem' }}>
                      <div className="position-absolute z-index-1 bg-white mx-2" style={{ left: 0, top: -10, paddingLeft: '1rem', paddingRight: '1rem' }}>
                        Send Asset Class
                      </div>
                      <Col className='mx-2 my-4'>
                        <Form.Control
                          name="sendCurrency"
                          type="text"
                          placeholder="Currency Symbol"
                        />
                      </Col>
                      <Col className='mx-2 my-4'>
                        <Form.Control
                          name="sendTokenName"
                          type="text"
                          placeholder="Token Name"
                        />
                      </Col>
                    </div>
                  </Col>
                  <Col sm={4} >
                    <div className="position-absolute" style={{ top: -10 }}>
                        Send Amount
                    </div>
                    {/* <Form.Label>Send Amount</Form.Label> */}
                    <Form.Control
                      name="sendAmount"
                      type="number"
                      placeholder="Amount"
                      className='my-4'
                    />
                  </Col>
                </Row>
              </div>
              <br></br>
              <div className="position-relative">
              <Row>
                <Col>
                  <div className='d-flex align-items-center' style={{ border: '1px solid black', borderRadius: '1rem' }}>
                    <div className="position-absolute z-index-1 bg-white mx-2" style={{ left: 0, top: -10, paddingLeft: '1rem', paddingRight: '1rem' }}>
                      Receive Asset Class
                    </div>
                    <Col className='mx-2 my-4'>
                      <Form.Control
                        name="recCurrency"
                        placeholder="Currency Symbol"
                      />
                    </Col>
                    <Col className='mx-2 my-4'>
                      <Form.Control
                        name="recTokenName"
                        placeholder="Token name"
                      />
                    </Col>
                  </div>
                </Col>
                <Col sm={4}>
                  <div className="position-absolute" style={{ top: -10 }}>
                    Receive Amount
                  </div>
                  <Form.Control
                    name="recAmount"
                    type="number"
                    placeholder="Amount"
                    className="my-4"
                  />
                </Col>
              </Row>
              </div>
              <br></br>
              <Row>
                  <div className="d-flex align-items-center">
                    <Button
                      variant="primary"
                      type="submit"
                      className="mx-auto"
                    >
                      Submit
                    </Button>
                  </div>
              </Row>
            </Form.Group>
          </Form>
        </Modal.Body>
      </Modal>
    </>
  )
}

type CancelProps = {
  showCancelModal: boolean
  setShowCancelModal: React.Dispatch<React.SetStateAction<boolean>>
  contractEndpoints: UserEndpoints
};

// Component that displays the form for canceling started escrows.
const Cancel = ({showCancelModal, setShowCancelModal, contractEndpoints}: CancelProps) => {
  const handleClose = e => {
    setShowCancelModal(false)
    e.preventDefault()
    const formData = new FormData(e.target),
      recAddr = formData.get("recAddr") as string,
      txOutRef = formData.get("txOutRef") as string

    mkCancelParams(recAddr, txOutRef)
      .then(cp => contractEndpoints.cancel(cp))
  }
  return (
    <>
    <Modal show={showCancelModal}>
        <Modal.Header closeButton onHide={ () => setShowCancelModal(false)}>
          <Modal.Title>Cancel an Escrow</Modal.Title>
        </Modal.Header>
        <Modal.Body>
          <Form onSubmit={handleClose}>
            <Form.Group className="mb-3" controlId="cancelForm">
              <Form.Label>Receiver Address</Form.Label>
              <Form.Control
                name="recAddr"
                placeholder="Address"
                autoFocus
              />
              <br></br>
              <Row>
                <Col>
                  <Form.Label>TxOutRef</Form.Label>
                  <Form.Control
                    name="txOutRef"
                    type="text"
                    placeholder="TxOutRef"
                  />
                </Col>
              </Row>
              <br></br>
              <Row>
                <Col></Col>
                <Col>
                  <Button
                    variant="primary"
                    type="submit"
                    style={{margin: "10px"}}
                  >
                    Cancel Escrow
                  </Button>
                </Col>
                <Col></Col>
              </Row>
            </Form.Group>
          </Form>
        </Modal.Body>
      </Modal>
    </>
  )
}

type ResolveProps = {
  txOutRefToResolve: TxOutRef
  contractEndpoints: UserEndpoints
}

// Component that handles the resolving of started escrows.
const Resolve = ({ txOutRefToResolve, contractEndpoints }: ResolveProps) => {
  return (
    <ButtonWithSpinner
      onClick={async () => {
        console.log("Resolving escrow for ref: ")
        console.log(txOutRefToResolve)
        const params = mkResolveParams(txOutRefToResolve)
        await contractEndpoints.resolve(params)
      }}
      isDisabled={false}
      text={"Resolve"}
    />
  )
}
type ContractInformationProps = {
  currentContractState: ObsState
  contractEndpoints: UserEndpoints
}
// Component that displays the started escrows in a table.
const ContractInformation = ({ currentContractState, contractEndpoints }: ContractInformationProps) => {
  return (
    <div
      style={{
        textAlign: "center"
      }}
    >
    {<Table striped bordered hover className="align-middle">
      <thead>
        <tr>
          <th>Sender Address</th>
          <th>Send Amount</th>
          <th>Send Asset</th>
          <th>Receive Amount</th>
          <th>Receive Asset</th>
          <th>Resolve</th>
        </tr>
      </thead>
      <tbody>
        {currentContractState.map(({ escrowInfo, escrowUtxo, escrowPayment }: UtxoEscrowInfo, i) => {
          return <tr key={i}>
            <td> {escrowInfo.sender.paymentPubKeyHash} </td>
            <td> {escrowPayment[1]} </td>
            <td> {escrowPayment[0].tokenName} </td>
            <td> {escrowInfo.rAmount} </td>
            <td> {escrowInfo.rAssetClass.tokenName} </td>
            <td>
              <Resolve
                txOutRefToResolve={escrowUtxo}
                contractEndpoints={contractEndpoints}
              />
            </td>
          </tr>
        })}
      </tbody>
    </Table>}
    </div>
  )
}

type ButtonWithSpinnerProps = {
  onClick: () => Promise<void>
  isDisabled : boolean
  text: string
}

const ButtonWithSpinner = ({onClick, isDisabled, text}: ButtonWithSpinnerProps) => {
  const [isSpinning, setIsSpinning] = useState(false);
  return (
    <Button
      style={{ marginRight: "16px" }}
      variant="success"
      size="sm"
      disabled={isDisabled}
      onClick={async e => {
        setIsSpinning(true)
        await onClick()
        setIsSpinning(false)
        }
      }
    >
      { isSpinning ?
          <Spinner
            as="span"
            animation="border"
            size="sm"
            role="status"
            aria-hidden="true"
            className="mx-2"
          /> :
          <div>{text}</div>
          }
    </Button>
  )
}

export default EscrowUI
