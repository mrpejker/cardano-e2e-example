import React, { useState } from "react"
import { Container, Navbar, Nav, Button, Modal, Form, Table } from "react-bootstrap";
import Row from 'react-bootstrap/Row';
import Col from 'react-bootstrap/Col';
import { UserEndpoints, ObsState, UtxoEscrowInfo } from "src/contractEndpoints/user";
import { getValueAmount, getValueAsset, mkStartParams, mkCancelParams, mkResolveParams } from "src/contractEndpoints/parameters";
import { CIP30WalletWrapper, ContractEndpoints } from "cardano-pab-client";

// Main component for the UserUI. It includes all the other components.
function UserUI() {
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
      <h2> Active Escrows </h2>
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
  const [selectedWallet, setSelectedWallet] = useState("eternl");
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
            setSelectedWallet(e.target.value)
          }}
          defaultValue={"eternl"}
        >
          <option value="nami">Nami</option>
          <option value="eternl">Eternl</option>
        </select>
        <Button
            style={{ marginRight: "16px" }}
            variant="success"
            size="sm"
            onClick={async e => {
                console.log("Connecting")
                const ce = await contractEndpoints.connect(selectedWallet)
                console.log("Connected")
                setContractEndpoints(ce)
                setIsConnected(true)
                const obsState = await ce.reload()
                setCurrentContractState(obsState)
              }
            }
        >
          Connect Wallet
        </Button>
      </Nav.Link>
    </Navbar.Collapse>
  )
}

// Reload component that reloads the contract state
const Reload = ({contractEndpoints, isConnected, setCurrentContractState}) => {
  return (
    <Button
      style={{ marginRight: "16px" }}
      variant="success"
      size="sm"
      disabled={!isConnected}
      onClick={async e => {
        console.log("Reloading")
        const obsState = await contractEndpoints.reload()
        setCurrentContractState(obsState)
        }
      }
    >
      Reload
    </Button>
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
      sendAsset = formData.get("sendAsset") as string,
      sendAmount = parseInt(formData.get("sendAmount") as string),
      recAsset = formData.get("recAsset") as string,
      recAmount = parseInt(formData.get("recAmount") as string)

    mkStartParams(recAddr, sendAsset, sendAmount, recAsset, recAmount)
      .then(sp => contractEndpoints.start(sp))
  }
  return (
    <>
      <Modal show={showStartModal}>
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
              <Row>
                <Col>
                  <Form.Label>Send AssetClass</Form.Label>
                  <Form.Control
                    name="sendAsset"
                    type="text"
                    placeholder="Asset"
                  />
                </Col>
                <Col>
                  <Form.Label>Send Amount</Form.Label>
                  <Form.Control
                    name="sendAmount"
                    type="number"
                    placeholder="Amount"
                  />
                </Col>
              </Row>
              <br></br>
              <Row>
                <Col>
                  <Form.Label>Receive AssetClass</Form.Label>
                  <Form.Control
                    name="recAsset"
                    placeholder="Asset"
                  />
                </Col>
                <Col>
                  <Form.Label>Receive Amount</Form.Label>
                  <Form.Control
                    name="recAmount"
                    type="number"
                    placeholder="Amount"
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
                    Submit
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

// Component that handles the resolving of started escrows.
const Resolve = () => {
  return (
    <>
    </>
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
        {currentContractState.map((elem: UtxoEscrowInfo, index) => (
          <tr key={index}>
            <td> {elem.escrowInfo.sender.waPayment.getPubKeyHash} </td>
            <td> {getValueAmount(elem.escrowValue)} </td>
            <td> {getValueAsset(elem.escrowValue)} </td>
            <td> {elem.escrowInfo.rAmount} </td>
            <td> {elem.escrowInfo.rAssetClass.unAssetClass[1].unTokenName} </td>
            <td >
                <Button
                  style={{ marginRight: "16px" }}
                  variant="success"
                  size="sm"
                  onClick={async e => {
                    console.log("Resolving")
                    console.log(elem)
                    const params = mkResolveParams(elem.escrowUtxo)
                    contractEndpoints.resolve(params)
                  }
                  }
                > Resolve
                </Button>
            </td>
          </tr>
        ))}
      </tbody>
    </Table>}
    </div>

  )
}

export default UserUI
